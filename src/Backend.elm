module Backend exposing (..)

import App
import Dict exposing (Dict)
import Html
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Types exposing (..)


type alias Model =
    BackendModel


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
            let
                sessionInfo =
                    Dict.get sessionId model.sessions
                        |> Maybe.map
                            (\session ->
                                { session
                                    | clients = Set.insert clientId session.clients
                                }
                            )
                        |> Maybe.withDefault
                            { clients = Set.singleton clientId }

                sessions1 =
                    model.sessions
                        |> Dict.insert sessionId sessionInfo

                ( sessions2, clients ) =
                    case Dict.get clientId model.clients of
                        Nothing ->
                            ( sessions1
                            , model.clients |> Dict.insert clientId { session = sessionId }
                            )

                        Just old ->
                            -- Not sure this can happen?
                            if old.session == sessionId then
                                ( sessions1
                                , model.clients
                                )

                            else
                                -- Even less sure this can happen
                                ( sessions1
                                    |> Dict.update old.session
                                        (Maybe.map
                                            (\session ->
                                                { session
                                                    | clients = Set.remove clientId session.clients
                                                }
                                            )
                                        )
                                , model.clients |> Dict.insert clientId { old | session = sessionId }
                                )
            in
            ( { model
                | clients = clients
                , sessions = sessions2
              }
            , Lamdera.sendToFrontend clientId (Props <| App.propsForUser model.model sessionId)
            )

        ClientDisconnected sessionId clientId ->
            ( case ( Dict.get sessionId model.sessions, Dict.get clientId model.clients ) of
                ( Just session, Just client ) ->
                    { model
                        | sessions =
                            model.sessions
                                |> Dict.insert clientId { session | clients = Set.remove clientId session.clients }
                        , clients = model.clients |> Dict.remove clientId
                    }

                _ ->
                    -- (Nothing, Nothing) implies a clientdisconnect without a preceding clientconnect
                    -- (Just session, Nothing) imples a clientdisconnect without a preceding clientconnect for that session
                    -- (Nothing, Just client) probably implies we made a mistake, and we'd be missing cleanup work
                    -- Unsure if any of these can happen
                    model
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
                    model.model |> App.updateModel sessionId bmsg
            in
            ( { model | model = model.model |> App.updateModel sessionId bmsg }
            , model.clients
                |> Dict.toList
                |> List.map
                    (\( client, { session } ) ->
                        Lamdera.sendToFrontend client (Props (App.propsForUser newModel session))
                    )
                |> Cmd.batch
            )


subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
