module Apps.Doodle exposing (..)

import Array exposing (Array)
import Color
import Css exposing (pct, px)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List


type alias Color =
    Char


pallate : List ( Color, { red : Int, green : Int, blue : Int } )
pallate =
    [ ( '#', { red = 0, green = 0, blue = 0 } )
    , ( ' ', { red = 255, green = 255, blue = 255 } )
    , ( 'R', { red = 108, green = 29, blue = 25 } )
    , ( 'B', { red = 36, green = 74, blue = 123 } )
    , ( 'Y', { red = 211, green = 178, blue = 85 } )
    ]


colorC : Color -> { red : Int, green : Int, blue : Int }
colorC c =
    pallate
        |> List.findMap
            (\( c2, rgb ) ->
                if c == c2 then
                    Just rgb

                else
                    Nothing
            )
        |> Maybe.withDefault { red = 255, green = 0, blue = 255 }


colorRGB : Color -> Css.Color
colorRGB c =
    let
        { red, green, blue } =
            colorC c
    in
    Css.rgb red green blue


type alias Model =
    { data : Array Color
    , width : Int
    , height : Int
    , owner : Maybe SessionId
    , locked : Bool
    }


type alias Props =
    { data : Array Color
    , width : Int
    , height : Int
    , owner : Bool
    , locked : Bool
    }


type alias State =
    { color : Color
    , drawing : Bool
    , grid : Bool
    , data : Array (Maybe Color)
    , locked : Bool
    }


deriveProps : SessionId -> ClientId -> Model -> Props
deriveProps sessionId _ { data, width, height, owner, locked } =
    { data = data
    , width = width
    , height = height
    , owner = Just sessionId == owner
    , locked = locked
    }


type Msg
    = Color { index : Int, color : Color }
    | ChooseColor Color
    | Deactivate
    | ToggleGrid
    | ToggleLock
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

        ToggleLock ->
            Just msg

        NoOpMsg ->
            Nothing


width_ =
    52


height_ =
    30


initModel : SessionId -> ClientId -> Model
initModel sessionId _ =
    { data =
        Array.initialize (width_ * height_)
            (\i ->
                if i < width_ || i >= (width_ * (height_ - 1)) then
                    '#'

                else if (i |> modBy width_) == 0 || (i + 1 |> modBy width_) == 0 then
                    '#'

                else
                    ' '
            )
    , width = width_
    , height = height_
    , owner = Just sessionId
    , locked = False
    }


initState : Props -> State
initState { data, locked } =
    { color = '#'
    , drawing = False
    , grid = False
    , data = Array.initialize (Array.length data) (always Nothing)
    , locked = locked
    }


updateModel : SessionId -> ClientId -> ToBackend -> Model -> Model
updateModel sessionId _ msg model =
    case msg of
        Color { index, color } ->
            if not model.locked || Just sessionId == model.owner then
                { model
                    | data =
                        model.data |> Array.set index color
                }

            else
                model

        ChooseColor _ ->
            model

        Deactivate ->
            model

        NoOpMsg ->
            model

        ToggleGrid ->
            model

        ToggleLock ->
            { model | locked = not model.locked }


updateState : Msg -> Props -> State -> State
updateState msg props state =
    case msg of
        Color { index, color } ->
            if props.locked && not props.owner then
                state

            else
                { state
                    | drawing = True
                    , data =
                        if Just color /= Array.get index props.data then
                            state.data |> Array.set index (Just color)

                        else
                            state.data |> Array.set index Nothing
                }

        ChooseColor color ->
            { state | color = color, drawing = False }

        Deactivate ->
            { state | drawing = False }

        NoOpMsg ->
            state

        ToggleGrid ->
            { state | grid = not state.grid }

        ToggleLock ->
            { state | locked = not state.locked }


receiveProps : { new : Props, old : Props } -> State -> State
receiveProps { new, old } state =
    { state
        | data =
            new.data
                |> Array.indexedMap
                    (\index color ->
                        if Just color == Array.get index old.data then
                            Array.get index state.data
                                |> Maybe.andThen identity

                        else
                            Nothing
                    )
        , drawing = (not new.locked || new.owner) && state.drawing
    }


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
            toFloat props.width

        height =
            toFloat props.height

        gridSize =
            if state.grid then
                1

            else
                0
    in
    Html.div
        [ Attr.css
            [ Css.width (px <| side * width + 10)
            , Css.margin4 (px 100) Css.auto (px 0) Css.auto
            , Css.boxSizing Css.borderBox
            ]
        ]
        [ Html.div
            [ Attr.css
                [ Css.position Css.relative
                , Css.height (px <| side * height)
                , Css.width (px <| side * width)
                , Css.marginBottom (px 5)
                , Css.border3 (px 5) Css.solid <|
                    if state.drawing then
                        active

                    else
                        gray
                ]
            ]
            (props.data
                |> Array.toList
                |> List.indexedMap
                    (\index propsColor ->
                        let
                            ( color, opacity ) =
                                case Array.get index state.data of
                                    Just (Just overlayColor) ->
                                        ( overlayColor, 0.7 )

                                    _ ->
                                        ( propsColor, 1 )

                            col =
                                index |> modBy props.width

                            row =
                                index // props.width
                        in
                        Html.div
                            (Attr.css
                                [ Css.position Css.absolute
                                , Css.top (px <| side * toFloat row)
                                , Css.left (px <| side * toFloat col)
                                , Css.width (px (side - gridSize))
                                , Css.height (px (side - gridSize))
                                , Css.backgroundColor (colorRGB color)
                                , Css.borderLeft3 (px gridSize) Css.solid border
                                , Css.borderTop3 (px gridSize) Css.solid border
                                , Css.opacity (Css.num opacity)
                                ]
                                :: (if props.locked && not props.owner then
                                        []

                                    else if not state.drawing then
                                        [ Events.onClick (Color { index = index, color = state.color })
                                        ]

                                    else if color == state.color then
                                        [ Events.onClick Deactivate
                                        ]

                                    else
                                        [ Events.onMouseEnter (Color { index = index, color = state.color })
                                        , Events.onClick Deactivate
                                        ]
                                   )
                            )
                            []
                    )
            )
        , Html.div [ Attr.css [ Css.marginBottom (px 5) ] ]
            (pallate
                |> List.map
                    (\( color, _ ) ->
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
        , toggle (always ToggleGrid) state.grid state.grid <|
            if state.grid then
                "Grid enabled"

            else
                "Grid disabled"
        , if props.owner then
            toggle (always ToggleLock) props.locked state.locked <|
                if props.locked then
                    "Locked: only you can edit"

                else
                    "Unlocked: anyone can edit"

          else
            Html.div []
                [ if props.locked then
                    Html.text "Locked: only the original creator can edit"

                  else
                    Html.text "Unlocked: anyone can edit"
                ]
        , Html.div [] [ Html.a [ Attr.href "/" ] [ Html.text "Go home" ] ]
        ]


toggle : (Bool -> Msg) -> Bool -> Bool -> String -> Html Msg
toggle msg props state text =
    let
        width__ =
            40

        height__ =
            24

        space__ =
            3

        t_height =
            height__ - space__ * 2

        gray =
            Css.rgb 100 100 100

        active =
            Css.rgb 17 234 234
    in
    Html.div
        [ Attr.css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.marginBottom <| px 5
            ]
        ]
        [ Html.label
            [ Attr.css
                [ Css.width <| px width__
                , Css.height <| px height__
                , Css.borderRadius <| px (height__ / 2)
                , Css.display Css.inlineBlock
                , Css.marginRight <| px 5
                , Css.position Css.relative
                , Css.backgroundColor <|
                    if props then
                        active

                    else
                        gray
                ]
            ]
            [ Html.input
                [ Attr.type_ "checkbox"
                , Attr.css
                    [ Css.opacity Css.zero
                    , Css.width <| px 0
                    , Css.height <| px 0
                    ]
                , Events.onCheck
                    (if props == state then
                        msg

                     else
                        always NoOpMsg
                    )
                ]
                []
            , Html.span
                [ Attr.css
                    [ Css.position Css.absolute
                    , Css.backgroundColor <| Css.rgb 255 255 255
                    , Css.width <| px t_height
                    , Css.height <| px t_height
                    , Css.borderRadius <| px (t_height / 2)
                    , Css.left <|
                        px
                            (if props == state then
                                if state then
                                    width__ - t_height - space__

                                else
                                    space__

                             else
                                width__ / 2 - t_height / 2
                            )
                    , Css.top <| px space__
                    ]
                ]
                []
            ]
        , Html.span []
            [ Html.text text ]
        ]
