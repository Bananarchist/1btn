module Main exposing (main)

import Browser
import Game
import Model exposing (Model)
import Msg exposing (Msg)


main : Program { width : Int, height : Int, currentTick : Float } Model Msg
main =
    Browser.element
        { init = Game.init
        , update = Game.update
        , subscriptions = Game.subscriptions
        , view = Game.view
        }
