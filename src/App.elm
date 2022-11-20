module App exposing (..)

import Apps.Doodle as App
import Html.Styled exposing (Html)
import Lamdera exposing (ClientId, SessionId)


{-| The model is the global state for the entire application.
-}
type alias Model =
    App.Model


initModel : Model
initModel =
    App.initModel


{-| Props are deterministically derived from the model by propsForUser.
This allows different clients to see different props.
-}
type alias Props =
    App.Props


{-| The state is local frontend state, and allows the application to react
to messages without waiting for a server round-trip.
-}
type alias State =
    App.State


initState : Props -> State
initState =
    App.initState


{-| In this model, there's just a single `Msg` type that exists on the
frontend. Every generated msg first has the opportunity to change the
State of the local app, and is then is forwarded on to the server where
it can influence the Model (and thereby the Props).
-}
type alias Msg =
    App.Msg


{-| This represents the message type that actually gets passed to the backend.
-}
type alias ToBackend =
    App.ToBackend


{-| Transforms a `Msg` into a `ToBackend`, allowing msgs to be only
selectively sent over the wire to where they can influence the Model.

If you don't want to bother, the easiest way to handle this is to send
all messages to the backend:

    type alias ToBackend =
        Msg

    toBackend msg =
        Just msg

-}
toBackend : Msg -> Maybe ToBackend
toBackend =
    App.toBackend


{-| Every local message has the opportunity to update the local state.
The state is updated before any message is communicated to the backend,
and so this is an opportunity to change the local state to register "I've
tried to send a message but it's not gonna be reflected in the props yet."
-}
updateState : Msg -> Props -> State -> State
updateState =
    App.updateState


{-| When the backend receives a msg, the backend has the opportunity to update
the model.
-}
updateModel : SessionId -> ClientId -> ToBackend -> Model -> Model
updateModel =
    App.updateModel


{-| Given a sessionId and a clientId, derive the props from the model.
-}
deriveProps : SessionId -> ClientId -> Model -> Props
deriveProps =
    App.deriveProps


{-| The best analogy here is the very-not-beloved componentWillReceiveProps
from React. The most Elm-like solution to avoiding uses of componentWillReceiveProps
in React is to have the component be
[fully controlled](https://reactjs.org/blog/2018/06/07/you-probably-dont-need-derived-state.html#recommendation-fully-controlled-component),
and we could do that here, but it would mean that every update would require an
RPC round-trip, which is going to feel sluggish in most situations. Therefore,
perhaps we want to resurrect the receiveProps idea in order to have controlled components?
-}
receiveProps : { new : Props, old : Props } -> State -> State
receiveProps =
    App.receiveProps


view : Props -> State -> Html Msg
view =
    App.view
