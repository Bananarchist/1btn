module Map exposing (updateMap, updateMapCmds)

import Msg exposing (Msg(..))
import Model exposing (Model)

updateMap : Msg -> Model -> Model
updateMap msg model =
    case msg of 
--        Msg.GrassGenerated values -> Model.setGrass values model
 --       Msg.GrassAnimations values -> Model.setGrassAnimations values model
        _ -> model

updateMapCmds : Msg -> Model -> List (Cmd Msg)
updateMapCmds msg model =
    case msg of
        Msg.Frame _ -> 
            {-          if Model.grassAnimationsCompleteか model then
                [ Msg.animateGrass 100 ]
            else -}
                []
        _ -> []
