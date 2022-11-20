module Evergreen.Migrate.V2 exposing (..)

import Evergreen.V1.Apps.Doodle
import Evergreen.V1.Types as Old
import Evergreen.V2.Apps.Doodle
import Evergreen.V2.Types as New
import Lamdera.Migrations exposing (..)
import Svg.Styled.Attributes exposing (x)


boolColor : Bool -> Evergreen.V2.Apps.Doodle.Color
boolColor b =
    if b then
        Evergreen.V2.Apps.Doodle.Black

    else
        Evergreen.V2.Apps.Doodle.White


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { key = old.key
          , mode =
                case old.mode of
                    Old.Waiting s ->
                        New.Waiting s

                    Old.App { props, state } ->
                        New.App
                            { props =
                                { grid =
                                    props.grid
                                        |> List.map (List.map boolColor)
                                }
                            , state =
                                { color = boolColor state.color
                                , drawing = state.drawing
                                }
                            }
          }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { sessions = old.sessions
          , clients = old.clients
          , model =
                { grid =
                    old.model.grid
                        |> List.map (List.map boolColor)
                }
          }
        , Cmd.none
        )


appMsg msg =
    case msg of
        Evergreen.V1.Apps.Doodle.Color { row, col, color } ->
            Evergreen.V2.Apps.Doodle.Color { row = row, col = col, color = boolColor color }

        Evergreen.V1.Apps.Doodle.ChooseColor b ->
            Evergreen.V2.Apps.Doodle.ChooseColor (boolColor b)

        Evergreen.V1.Apps.Doodle.Click ->
            Evergreen.V2.Apps.Doodle.Deactivate

        Evergreen.V1.Apps.Doodle.NoOpMsg ->
            Evergreen.V2.Apps.Doodle.NoOpMsg


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgMigrated
        ( case old of
            Old.UrlClicked x ->
                New.UrlClicked x

            Old.UrlChanged x ->
                New.UrlChanged x

            Old.NoOpFrontendMsg ->
                New.NoOpFrontendMsg

            Old.Spinner x ->
                New.Spinner x

            Old.AppMsg msg ->
                New.AppMsg (appMsg msg)
        , Cmd.none
        )


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgMigrated
        ( case old of
            Old.NoOpToBackend ->
                New.NoOpToBackend

            Old.AppToBackend msg ->
                New.AppToBackend (appMsg msg)
        , Cmd.none
        )


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgMigrated
        ( case old of
            Old.NoOpBackendMsg ->
                New.NoOpBackendMsg

            Old.ClientConnected s c ->
                New.ClientConnected s c

            Old.ClientDisconnected s c ->
                New.ClientDisconnected s c
        , Cmd.none
        )


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend _ =
    MsgOldValueIgnored
