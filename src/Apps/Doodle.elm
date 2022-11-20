module Apps.Doodle exposing (..)

import Css exposing (px)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List


type alias Model =
    { grid : List (List Bool) }


type alias Props =
    Model


type alias State =
    { color : Bool, drawing : Bool }


deriveProps : SessionId -> ClientId -> Model -> Props
deriveProps _ _ model =
    model


type Msg
    = Color { row : Int, col : Int, color : Bool }
    | ChooseColor Bool
    | Click
    | NoOpMsg


initModel : Model
initModel =
    { grid =
        List.repeat 30 (List.repeat 52 False)
            |> List.updateAt 2 (List.updateAt 7 (always True))
            |> List.updateAt 2 (List.updateAt 8 (always True))
            |> List.updateAt 2 (List.updateAt 9 (always True))
            |> List.updateAt 3 (List.updateAt 9 (always True))
    }


initState : Props -> State
initState _ =
    { color = True, drawing = False }


updateModel : SessionId -> ClientId -> Msg -> Model -> Model
updateModel _ _ msg model =
    case msg of
        Color { row, col, color } ->
            { model
                | grid =
                    model.grid |> List.updateAt row (List.updateAt col (always color))
            }

        ChooseColor _ ->
            model

        Click ->
            model

        NoOpMsg ->
            model


updateState : Msg -> Props -> State -> State
updateState msg _ state =
    case msg of
        Color _ ->
            state

        ChooseColor color ->
            { state | color = color }

        Click ->
            { state | drawing = not state.drawing }

        NoOpMsg ->
            state


receiveProps : { new : Props, old : Props } -> State -> State
receiveProps _ state =
    state


view : Props -> State -> Html Msg
view props state =
    let
        side =
            10

        black =
            Css.rgb 0 0 0

        white =
            Css.rgb 255 255 255

        gray =
            Css.rgb 100 100 100
    in
    Html.div [ Attr.css [ Css.width (px 520), Css.margin4 (px 100) Css.auto (px 0) Css.auto ] ]
        [ Html.div [ Attr.css [ Css.position Css.relative, Css.height (px <| side * toFloat (1 + List.length props.grid)) ] ]
            (props.grid
                |> List.indexedMap
                    (\row ->
                        List.indexedMap
                            (\col color ->
                                Html.div
                                    [ Attr.css
                                        [ Css.position Css.absolute
                                        , Css.top (px <| side * toFloat row)
                                        , Css.left (px <| side * toFloat col)
                                        , Css.width (px side)
                                        , Css.height (px side)
                                        , Css.backgroundColor
                                            (if color then
                                                black

                                             else
                                                white
                                            )
                                        ]
                                    , Events.onMouseEnter
                                        (if state.drawing then
                                            Color { row = row, col = col, color = state.color }

                                         else
                                            NoOpMsg
                                        )
                                    , Events.onClick Click
                                    ]
                                    []
                            )
                    )
                |> List.concat
            )
        , Html.div [ Events.onClick (ChooseColor True) ]
            [ Html.div [ Attr.css [ Css.display Css.inlineBlock, Css.width (px 20), Css.height (px 20), Css.backgroundColor black, Css.border3 (px 2) Css.solid gray ] ] []
            , Html.text
                (if state.color == True then
                    "selected"

                 else
                    ""
                )
            ]
        , Html.div [ Events.onClick (ChooseColor False) ]
            [ Html.div [ Attr.css [ Css.display Css.inlineBlock, Css.width (px 20), Css.height (px 20), Css.backgroundColor white, Css.border3 (px 2) Css.solid gray ] ] []
            , Html.text
                (if state.color == False then
                    "selected"

                 else
                    ""
                )
            ]
        ]
