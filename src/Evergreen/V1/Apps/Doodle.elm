module Evergreen.V1.Apps.Doodle exposing (..)


type alias Model =
    { grid : List (List Bool)
    }


type alias Props =
    Model


type alias State =
    { color : Bool
    , drawing : Bool
    }


type Msg
    = Color
        { row : Int
        , col : Int
        , color : Bool
        }
    | ChooseColor Bool
    | Click
    | NoOpMsg
