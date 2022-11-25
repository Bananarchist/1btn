module Moth exposing (updateMoth, updateMothCmds)

import Msg exposing (Msg(..))
import Model exposing (Model)

updateMoth : Msg -> Model -> Model
updateMoth msg model =
    case msg of
        Msg.Frame t -> 
            Model.updateMothStatus model
        Msg.MothFlutterings values ->
            Model.setMothMovements values model
        _ -> model

updateMothCmds : Msg -> Model -> List (Cmd Msg)
updateMothCmds msg model =
    if Model.mothFartingか model || Model.anyPendingMothMovementsか model then
        []
    else 
        [ Msg.flutterMoth ]
