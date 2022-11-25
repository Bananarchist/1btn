port module Msg exposing
    ( Msg(..)
    , animationFrameSub, padButtonsSub, padConnectionSub, visibilitySub, keyboardSub, mouseSub, resizeSub
    , pollPad, startFart, stopFart, generateGrass, initializeAudio, flutterMoth, initializeTime
    , tNewKeyPress, tNewKeyRelease
    , animateGrass
    )

{-| Msg type and API


## Type

@docs Msg


## Subs

@docs animationFrameSub, padButtonsSub, padConnectionSub, visibilitySub, keyboardSub, mouseSub, resizeSub


## Msgs

@docs pollPad, startFart, stopFart, generateGrass, initializeAudio, flutterMoth, initializeTime


## Selectors

@docs tNewKeyPress, tNewKeyRelease

-}

import Browser.Events
import Duration
import Json.Decode exposing (succeed)
import Model exposing (Grass)
import Random
import Seeds
import Time
import Task


type Msg
    = NoOp
      --| System
    | ReceivedVisibility
    | LostVisibility
    | ResizedWindow Int Int
    | InitialTick Duration.Duration
      --| Graphics
    | Frame Duration.Duration
      --| Gamepad
    | PadButtonsDown
    | PadButtonsReleased
    | PadConnected Int
    | PadDisconnected Int
      --| Keyboard
    | KeyboardDown
    | KeyboardReleased
      --| Mouse
    | MouseDown
    | MouseReleased
      --| Random values
    | GrassGenerated (List ( Bool, Grass ))
    | GrassAnimations (List Bool)
    | MothFlutterings (List Seeds.MothFluttering)


{-| Helpers
-}
posixToDuration : Time.Posix -> Duration.Duration
posixToDuration =
    Time.posixToMillis >> toFloat >> Duration.milliseconds


{-| System
-}
visibilitySub : Sub Msg
visibilitySub =
    Browser.Events.onVisibilityChange
        (\vis ->
            case vis of
                Browser.Events.Hidden ->
                    LostVisibility

                Browser.Events.Visible ->
                    ReceivedVisibility
        )


resizeSub : Sub Msg
resizeSub =
    Browser.Events.onResize ResizedWindow


initializeTime : Cmd Msg
initializeTime =
    Task.perform (posixToDuration >> InitialTick) Time.now


{-| Graphics
-}
animationFrameSub : Sub Msg
animationFrameSub =
    Browser.Events.onAnimationFrame (posixToDuration >> Frame)


{-| Controls
-}
port pollPadButtons : () -> Cmd msg


port padButtonsDown : (Bool -> msg) -> Sub msg


port padConnected : (Int -> msg) -> Sub msg


port padDisconnected : (Int -> msg) -> Sub msg


pollPad : Cmd msg
pollPad =
    pollPadButtons ()


padButtonsSub : Sub Msg
padButtonsSub =
    padButtonsDown
        (\down ->
            if down then
                PadButtonsDown

            else
                PadButtonsReleased
        )


padConnectionSub : Sub Msg
padConnectionSub =
    Sub.batch [ padConnected PadConnected, padDisconnected PadDisconnected ]


keyboardSub : Sub Msg
keyboardSub =
    Sub.batch [ Browser.Events.onKeyDown (succeed KeyboardDown), Browser.Events.onKeyUp (succeed KeyboardReleased) ]


mouseSub : Sub Msg
mouseSub =
    Sub.batch [ Browser.Events.onMouseDown (succeed MouseDown), Browser.Events.onMouseUp (succeed MouseReleased) ]


tNewKeyPress : Msg -> Bool
tNewKeyPress msg =
    case msg of
        PadButtonsDown ->
            True

        MouseDown ->
            True

        KeyboardDown ->
            True

        _ ->
            False


tNewKeyRelease : Msg -> Bool
tNewKeyRelease msg =
    case msg of
        PadButtonsReleased ->
            True

        MouseReleased ->
            True

        KeyboardReleased ->
            True

        _ ->
            False


{-| Audio
-}
port initializeAudioContext : () -> Cmd msg


port startFartSFX : () -> Cmd msg


port stopFartSFX : () -> Cmd msg


initializeAudio : Cmd Msg
initializeAudio =
    initializeAudioContext ()


startFart : Cmd Msg
startFart =
    startFartSFX ()


stopFart : Cmd Msg
stopFart =
    stopFartSFX ()


{-| Random value generators
-}
generateGrass : Int -> Cmd Msg
generateGrass units =
    Random.list units Seeds.grassSeed
        |> Random.generate GrassGenerated


animateGrass : Int -> Cmd Msg
animateGrass units =
    Random.list units Seeds.grassAnimationSeed
        |> Random.generate GrassAnimations


flutterMoth : Cmd Msg
flutterMoth =
    Random.list 4 Seeds.flutterMovements
        |> Random.generate MothFlutterings
