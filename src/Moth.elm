module Moth exposing (updateMoth, updateMothCmds)

import Angle exposing (Angle)
import Constants exposing (Position, ScreenPosition, Direction, metersPerSecondOfFarting)
import Point2d
import Direction2d
import Duration exposing (Duration)
import Quantity

import Msg exposing (Msg(..))
import Model exposing (Model)

updateMoth : Msg -> Model -> Model
updateMoth msg model =
    case msg of
        Msg.Frame t -> 
            Model.update model
        Msg.MothFlutterings values ->
            Model.setIdleMovements values model
        _ -> model

updateMothCmds : Msg -> Model -> List (Cmd Msg)
updateMothCmds msg model =
    if Model.mothFartingか model || Model.mothPendingMovementsか model then
        []
    else 
        [ Msg.flutterMoth ]

