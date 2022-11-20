module Apps.Doodle exposing (..)

import Css exposing (px)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List


type Color
    = Red
    | Blue
    | Yellow
    | Black
    | White


colorRGB : Color -> Css.Color
colorRGB c =
    case c of
        Red ->
            Css.rgb 108 29 25

        Blue ->
            Css.rgb 36 74 123

        Yellow ->
            Css.rgb 211 178 85

        Black ->
            Css.rgb 0 0 0

        White ->
            Css.rgb 255 255 255


type alias Model =
    { grid : List (List Color) }


type alias Props =
    Model


type alias State =
    { color : Color, drawing : Bool, grid : Bool }


deriveProps : SessionId -> ClientId -> Model -> Props
deriveProps _ _ model =
    model


type Msg
    = Color { row : Int, col : Int, color : Color }
    | ChooseColor Color
    | Deactivate
    | ToggleGrid
    | NoOpMsg


type alias ToBackend =
    Msg


toBackend : Msg -> Maybe ToBackend
toBackend msg =
    case msg of
        Color _ ->
            Just msg

        ChooseColor _ ->
            Nothing

        Deactivate ->
            Nothing

        ToggleGrid ->
            Nothing

        NoOpMsg ->
            Nothing


initModel : Model
initModel =
    { grid =
        List.repeat 30 (List.repeat 52 White)
            |> List.updateAt 2 (List.updateAt 7 (always Red))
            |> List.updateAt 2 (List.updateAt 8 (always Blue))
            |> List.updateAt 2 (List.updateAt 9 (always Yellow))
            |> List.updateAt 3 (List.updateAt 9 (always Black))
    }


initState : Props -> State
initState _ =
    { color = Black, drawing = False, grid = False }


updateModel : SessionId -> ClientId -> ToBackend -> Model -> Model
updateModel _ _ msg model =
    case msg of
        Color { row, col, color } ->
            { model
                | grid =
                    model.grid |> List.updateAt row (List.updateAt col (always color))
            }

        ChooseColor _ ->
            model

        Deactivate ->
            model

        NoOpMsg ->
            model

        ToggleGrid ->
            model


updateState : Msg -> Props -> State -> State
updateState msg _ state =
    case msg of
        Color _ ->
            { state | drawing = True }

        ChooseColor color ->
            { state | color = color, drawing = False }

        Deactivate ->
            { state | drawing = False }

        NoOpMsg ->
            state

        ToggleGrid ->
            { state | grid = not state.grid }


receiveProps : { new : Props, old : Props } -> State -> State
receiveProps _ state =
    state


view : Props -> State -> Html Msg
view props state =
    let
        side =
            10

        white =
            Css.rgb 255 255 255

        gray =
            Css.rgb 100 100 100

        border =
            Css.rgb 200 200 200

        active =
            Css.rgb 17 234 234

        width =
            side * (toFloat <| List.length (List.head props.grid |> Maybe.withDefault []))
    in
    Html.div
        [ Attr.css
            [ Css.width (px <| width + 10)
            , Css.margin4 (px 100) Css.auto (px 0) Css.auto
            ]
        ]
        [ Html.div
            [ Attr.css
                [ Css.position Css.relative
                , Css.height (px <| side * toFloat (List.length props.grid))
                , Css.width (px <| width)
                , Css.marginBottom (px 5)
                , Css.border3 (px 5) Css.solid <|
                    if state.drawing then
                        active

                    else
                        gray
                ]
            ]
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
                                        , Css.backgroundColor (colorRGB color)
                                        , Css.border3
                                            (if state.grid then
                                                px 1

                                             else
                                                px 0
                                            )
                                            Css.solid
                                            border
                                        ]
                                    , Events.onMouseEnter
                                        (if state.drawing then
                                            Color { row = row, col = col, color = state.color }

                                         else
                                            NoOpMsg
                                        )
                                    , Events.onClick
                                        (if state.drawing then
                                            Deactivate

                                         else
                                            Color { row = row, col = col, color = state.color }
                                        )
                                    ]
                                    []
                            )
                    )
                |> List.concat
            )
        , Html.div [ Attr.css [ Css.marginBottom (px 5) ] ]
            ([ Black, White, Red, Yellow, Blue ]
                |> List.map
                    (\color ->
                        Html.div
                            [ Events.onClick (ChooseColor color)
                            , Attr.css
                                [ Css.display Css.inlineBlock
                                , Css.width (px 45)
                                , Css.height (px 45)
                                , Css.backgroundColor (colorRGB color)
                                , Css.border3 (px 5) Css.solid <|
                                    if color /= state.color then
                                        white

                                    else if state.drawing then
                                        active

                                    else
                                        gray
                                ]
                            ]
                            []
                    )
            )
        , Html.button [ Events.onClick ToggleGrid ] [ Html.text "Toggle grid" ]
        ]
