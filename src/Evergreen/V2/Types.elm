module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V2.App
import Lamdera
import Set
import Spinner
import Url


type Mode
    = Waiting Spinner.Model
    | App
        { props : Evergreen.V2.App.Props
        , state : Evergreen.V2.App.State
        }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , mode : Mode
    }


type alias Session =
    { clients : Set.Set Lamdera.ClientId
    }


type alias BackendModel =
    { sessions : Dict.Dict Lamdera.SessionId Session
    , clients :
        Dict.Dict
            Lamdera.ClientId
            { session : Lamdera.SessionId
            }
    , model : Evergreen.V2.App.Model
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AppMsg Evergreen.V2.App.Msg
    | NoOpFrontendMsg
    | Spinner Spinner.Msg


type ToBackend
    = NoOpToBackend
    | AppToBackend Evergreen.V2.App.ToBackend


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | Props Evergreen.V2.App.Props
