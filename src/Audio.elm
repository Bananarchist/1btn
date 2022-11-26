module Audio exposing (updateAudioCmds)

import Msg exposing (Msg(..))
import Model exposing (Model)
import Helpers exposing (consIf)

updateAudioCmds : Msg -> Model -> List (Cmd Msg)
updateAudioCmds msg model =
    updateFarts msg model
    ++ updateLizard msg model
    ++ updateBird msg model
    ++ updateSlurps msg model
    ++ playMusic msg model


updateFarts : Msg -> Model -> List (Cmd Msg)
updateFarts msg model =
    [ Cmd.none ]
    |> consIf (Msg.tNewKeyPress msg && Model.mothFartingか model) Msg.startFart
    |> consIf (Msg.tNewKeyRelease msg) Msg.stopFart
    |> consIf (Model.anyPressedか model |> not) Msg.stopFart

updateLizard : Msg -> Model -> List (Cmd Msg)
updateLizard = always <| always [ Cmd.none ]

updateBird : Msg -> Model -> List (Cmd Msg)
updateBird = always <| always [ Cmd.none ]

updateSlurps : Msg -> Model -> List (Cmd Msg)
updateSlurps = always <| always [ Cmd.none ]

playMusic : Msg -> Model -> List (Cmd Msg)
playMusic = always <| always [ Msg.startBackgroundMusic ]
