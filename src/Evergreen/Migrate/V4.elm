module Evergreen.Migrate.V4 exposing (..)

import Evergreen.V2.Apps.Doodle
import Evergreen.V2.Types as Old
import Evergreen.V4.Apps.Doodle
import Evergreen.V4.Types as New
import Lamdera.Migrations exposing (..)


updateColor msg =
    case msg of
        Evergreen.V2.Apps.Doodle.White ->
            Evergreen.V4.Apps.Doodle.White

        Evergreen.V2.Apps.Doodle.Black ->
            Evergreen.V4.Apps.Doodle.Black

        Evergreen.V2.Apps.Doodle.Red ->
            Evergreen.V4.Apps.Doodle.Red

        Evergreen.V2.Apps.Doodle.Blue ->
            Evergreen.V4.Apps.Doodle.Blue

        Evergreen.V2.Apps.Doodle.Yellow ->
            Evergreen.V4.Apps.Doodle.Yellow


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
                                        |> List.map (List.map updateColor)
                                }
                            , state =
                                { color = updateColor state.color
                                , drawing = state.drawing
                                , grid = True
                                }
                            }
          }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


appMsg msg =
    case msg of
        Evergreen.V2.Apps.Doodle.Color { row, col, color } ->
            Evergreen.V4.Apps.Doodle.Color { row = row, col = col, color = updateColor color }

        Evergreen.V2.Apps.Doodle.ChooseColor color ->
            Evergreen.V4.Apps.Doodle.ChooseColor (updateColor color)

        Evergreen.V2.Apps.Doodle.Deactivate ->
            Evergreen.V4.Apps.Doodle.Deactivate

        Evergreen.V2.Apps.Doodle.NoOpMsg ->
            Evergreen.V4.Apps.Doodle.NoOpMsg


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
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
