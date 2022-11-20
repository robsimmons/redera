module Backend exposing (..)

import App
import Dict
import Lamdera exposing (ClientId, SessionId)
import Set
import Types exposing (..)


type alias Model =
    BackendModel


app : { init : ( Model, Cmd BackendMsg ), update : BackendMsg -> Model -> ( Model, Cmd BackendMsg ), updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg ), subscriptions : Model -> Sub BackendMsg }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { sessions = Dict.empty
      , clients = Dict.empty
      , model = App.initModel
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        ClientConnected sessionId clientId ->
            ( { model
                | sessions =
                    model.sessions
                        |> Dict.update sessionId
                            (\maybeSession ->
                                maybeSession
                                    |> Maybe.map (\session -> { session | clients = Set.insert clientId session.clients })
                                    |> Maybe.withDefault { clients = Set.singleton clientId }
                                    |> Just
                            )
                , clients =
                    model.clients
                        |> Dict.insert clientId { session = sessionId }
              }
            , Lamdera.sendToFrontend clientId (Props <| App.deriveProps sessionId clientId model.model)
            )

        ClientDisconnected sessionId clientId ->
            ( { model
                | sessions =
                    model.sessions
                        |> Dict.update sessionId (Maybe.map (\session -> { session | clients = Set.remove clientId session.clients }))
                , clients =
                    model.clients
                        |> Dict.remove clientId
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        AppToBackend bmsg ->
            let
                newModel =
                    model.model |> App.updateModel sessionId clientId bmsg
            in
            ( { model | model = model.model |> App.updateModel sessionId clientId bmsg }
            , model.clients
                |> Dict.toList
                |> List.map
                    (\( client, { session } ) ->
                        Lamdera.sendToFrontend client (Props (App.deriveProps session client newModel))
                    )
                |> Cmd.batch
            )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
