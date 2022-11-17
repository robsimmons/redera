module Frontend exposing (..)

import App
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Color
import Html
import Html.Attributes as Attr
import Html.Styled
import Lamdera
import Spinner exposing (defaultConfig)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    case model.mode of
        Waiting spinner ->
            Spinner.subscription |> Sub.map Spinner

        App _ ->
            Sub.none


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , mode = Waiting Spinner.init
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        AppMsg amsg ->
            ( { model
                | mode =
                    case model.mode of
                        App { props, state } ->
                            App { props = props, state = App.updateState state amsg }

                        _ ->
                            model.mode
              }
            , case App.backendMsg amsg of
                Nothing ->
                    Cmd.none

                Just bmsg ->
                    Lamdera.sendToBackend (AppToBackend bmsg)
            )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        Spinner smsg ->
            ( { model
                | mode =
                    case model.mode of
                        Waiting spinner ->
                            Waiting (Spinner.update smsg spinner)

                        _ ->
                            model.mode
              }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        Props props ->
            ( { model
                | mode =
                    case model.mode of
                        Waiting _ ->
                            App { props = props, state = App.initState props }

                        App old ->
                            App { props = props, state = App.receiveProps old.props props old.state }
              }
            , Cmd.none
            )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ case model.mode of
            Waiting spinner ->
                Spinner.view
                    { defaultConfig
                        | width = 4
                        , radius = 12
                        , length = 15
                        , shadow = False
                        , color = always Color.darkGray
                    }
                    spinner

            App { props, state } ->
                App.view props state
                    |> Html.Styled.toUnstyled
                    |> Html.map AppMsg
        ]
    }
