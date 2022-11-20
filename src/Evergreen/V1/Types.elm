module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V1.App
import Lamdera
import Set
import Spinner
import Url


type Mode
    = Waiting Spinner.Model
    | App
        { props : Evergreen.V1.App.Props
        , state : Evergreen.V1.App.State
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
    , model : Evergreen.V1.App.Model
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AppMsg Evergreen.V1.App.Msg
    | NoOpFrontendMsg
    | Spinner Spinner.Msg


type ToBackend
    = NoOpToBackend
    | AppToBackend Evergreen.V1.App.Msg


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | Props Evergreen.V1.App.Props
