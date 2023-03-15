module Controls exposing (controllerSubs, updateControlCmds, updateControls)

import Helpers exposing (allPredicates, buildl, concatIf)
import Model exposing (Model)
import Msg exposing (Msg(..))


{-| Selectors
-}
listeningControlsか : Model -> Bool
listeningControlsか =
    Model.listeningか


listeningPadか : Model -> Bool
listeningPadか =
    allPredicates
        [ Model.listeningか
        , .system >> .controls >> .padCount >> (/=) 0
        ]


{-| Updaters
-}
updateControls : Msg -> Model -> Model
updateControls msg model =
    if listeningControlsか model then
        case msg of
            Msg.PadButtonsDown ->
                padPressed model

            Msg.PadButtonsReleased ->
                padDepressed model

            Msg.PadConnected _ ->
                listenForPad model

            Msg.PadDisconnected connectedPads ->
                stopListeningToPad connectedPads model

            Msg.KeyboardDown ->
                keyboardPressed model

            Msg.KeyboardReleased ->
                keyboardDepressed model

            Msg.MouseDown ->
                mousePressed model

            Msg.MouseReleased ->
                mouseDepressed model

            Msg.LostVisibility ->
                stopListeningForControls model

            _ ->
                model

    else
        case msg of
            Msg.ReceivedVisibility ->
                listenForControls model

            _ ->
                model


padPressed =
    Model.setPad True


padDepressed =
    Model.setPad False


listenForPad =
    Model.setListeningPad True



-->> Model.addNotification "Game pad connected"


stopListeningToPad connectedPads =
    if connectedPads > 0 then
        Model.setListeningPad True
        -->>  (Model.addNotification "Game pad connected")

    else
        Model.setListeningPad False



-->>  (Model.addNotification "Game pad disconnected")


keyboardPressed =
    Model.setKeyboard True


keyboardDepressed =
    Model.setKeyboard False


mousePressed =
    Model.setMouse True


mouseDepressed =
    Model.setMouse False


listenForControls =
    Model.setListeningControls True


stopListeningForControls =
    Model.setListeningControls False
        >> clearControls


clearControls =
    Model.setKeyboard False
        >> Model.setMouse False
        >> Model.setPad False


updateControlCmds : Msg -> Model -> List (Cmd Msg)
updateControlCmds msg model =
    case msg of
        Msg.Frame _ ->
            if listeningPadか model then
                [ Msg.pollPad ]

            else
                [ Cmd.none ]

        _ ->
            [ Cmd.none ]


controllerSubs : Model -> List (Sub Msg)
controllerSubs model =
    [ Msg.padConnectionSub ]
        |> concatIf (listeningControlsか model)
            [ Msg.padButtonsSub
            , Msg.keyboardSub
            , Msg.mouseSub
            ]
