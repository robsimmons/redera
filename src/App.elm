module App exposing (..)

import Css exposing (px)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Lamdera exposing (SessionId)
import List.Extra as List


type alias Entry =
    { owner : Maybe SessionId, contents : String }


type alias Model =
    { entries : List Entry }


type alias BackendModel =
    { model : Model
    }


type alias Props =
    { entries : List { owner : Maybe Bool, contents : String }
    }


type alias State =
    { contents : String
    , editable : Bool
    }


propsForUser : Model -> SessionId -> Props
propsForUser { entries } sessionId =
    { entries =
        entries
            |> List.map
                (\entry ->
                    case entry.owner of
                        Nothing ->
                            { owner = Nothing, contents = entry.contents }

                        Just owner ->
                            if owner == sessionId then
                                { owner = Just True, contents = entry.contents }

                            else
                                { owner = Just False, contents = entry.contents }
                )
    }


type ToBackend
    = Claim Int
    | Release
    | Update String


type Msg
    = Backend ToBackend
    | NoOpMsg


initModel : Model
initModel =
    { entries =
        [ { owner = Nothing, contents = "one" }
        , { owner = Nothing, contents = "two" }
        , { owner = Nothing, contents = "three" }
        , { owner = Nothing, contents = "four" }
        ]
    }


initState : Props -> State
initState { entries } =
    { contents =
        entries
            |> List.filterMap
                (\{ owner, contents } ->
                    if owner == Just True then
                        Just contents

                    else
                        Nothing
                )
            |> List.head
            |> Maybe.withDefault ""
    , editable = True
    }


backendMsg : Msg -> Maybe ToBackend
backendMsg msg =
    case msg of
        Backend msg2 ->
            Just msg2

        _ ->
            Nothing


updateModel : SessionId -> ToBackend -> Model -> Model
updateModel sessionId msg model =
    case msg of
        Update str ->
            { model
                | entries =
                    model.entries
                        |> List.map
                            (\entry ->
                                if entry.owner == Just sessionId then
                                    { entry | contents = str }

                                else
                                    entry
                            )
            }

        Claim i ->
            { model
                | entries =
                    model.entries
                        |> List.indexedMap
                            (\j entry ->
                                if i == j then
                                    { entry | owner = Just sessionId }

                                else if entry.owner == Just sessionId then
                                    { entry | owner = Nothing }

                                else
                                    entry
                            )
            }

        Release ->
            { model
                | entries =
                    model.entries
                        |> List.map
                            (\entry ->
                                if entry.owner == Just sessionId then
                                    { entry | owner = Nothing }

                                else
                                    entry
                            )
            }


updateState : State -> Msg -> State
updateState state msg =
    case msg of
        NoOpMsg ->
            state

        Backend (Update str) ->
            { state | contents = str }

        Backend (Claim _) ->
            { state | editable = False }

        Backend Release ->
            { state | editable = False }


receiveProps : Props -> Props -> State -> State
receiveProps oldProps newProps state =
    let
        oldEditableIndex =
            oldProps.entries |> List.findIndex (\{ owner } -> owner == Just True)

        newEditableIndex =
            newProps.entries |> List.findIndex (\{ owner } -> owner == Just True)
    in
    if oldEditableIndex /= newEditableIndex then
        { state
            | editable = True
            , contents =
                newEditableIndex
                    |> Maybe.andThen (\index -> List.getAt index newProps.entries)
                    |> Maybe.map .contents
                    |> Maybe.withDefault ""
        }

    else
        state


view : Props -> State -> Html Msg
view props state =
    Html.div [ Attr.css [ Css.width (px 520), Css.margin4 (px 100) Css.auto (px 0) Css.auto ] ] <|
        (props.entries
            |> List.indexedMap
                (\i { owner, contents } ->
                    Html.div [] <|
                        ((case owner of
                            Nothing ->
                                [ Html.button
                                    [ Attr.css [ Css.width (px 100) ]
                                    , Events.onClick (Backend (Claim i))
                                    ]
                                    [ Html.text "Claim" ]
                                ]

                            Just False ->
                                [ Html.button [ Attr.css [ Css.width (px 100) ], Attr.disabled True ] [ Html.text "(Reserved)" ] ]

                            Just True ->
                                [ Html.button
                                    [ Attr.css [ Css.width (px 100) ]
                                    , Events.onClick (Backend Release)
                                    ]
                                    [ Html.text "Release" ]
                                ]
                         )
                            ++ (case owner of
                                    Just True ->
                                        [ Html.input
                                            [ Attr.css [ Css.margin4 (px 0) (px 0) (px 0) (px 10), Css.width (Css.px 390) ], Attr.disabled (not state.editable), Attr.value state.contents, Events.onInput (Backend << Update) ]
                                            []
                                        ]

                                    _ ->
                                        [ Html.input
                                            [ Attr.css [ Css.margin4 (px 0) (px 0) (px 0) (px 10), Css.width (Css.px 390) ]
                                            , Attr.disabled True
                                            , Attr.value contents
                                            ]
                                            []
                                        ]
                               )
                        )
                )
        )
