module Evergreen.V4.Apps.Doodle exposing (..)


type Color
    = Red
    | Blue
    | Yellow
    | Black
    | White


type alias Model =
    { grid : List (List Color)
    }


type alias Props =
    Model


type alias State =
    { color : Color
    , drawing : Bool
    , grid : Bool
    }


type Msg
    = Color
        { row : Int
        , col : Int
        , color : Color
        }
    | ChooseColor Color
    | Deactivate
    | ToggleGrid
    | NoOpMsg


type alias ToBackend =
    Msg
