module Types exposing (..)

import App
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Spinner
import Url exposing (Url)


type Mode
    = Waiting Spinner.Model
    | App { props : App.Props, state : App.State }


type alias FrontendModel =
    { key : Key
    , mode : Mode
    }


type alias Session =
    { clients : Set ClientId }


type alias BackendModel =
    { sessions : Dict SessionId Session
    , clients : Dict ClientId { session : SessionId }
    , model : App.Model
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | AppMsg App.Msg
    | NoOpFrontendMsg
    | Spinner Spinner.Msg


type ToBackend
    = NoOpToBackend
    | AppToBackend App.Msg


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | Props App.Props
