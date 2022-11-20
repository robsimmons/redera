module Evergreen.V2.Apps.Doodle exposing (..)


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
    }


type Msg
    = Color
        { row : Int
        , col : Int
        , color : Color
        }
    | ChooseColor Color
    | Deactivate
    | NoOpMsg


type alias ToBackend =
    Msg
