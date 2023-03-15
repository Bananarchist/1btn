module NPCs exposing (updateNPCs)

import Msg exposing (Msg(..))
import Model exposing (Model)

updateNPCs : Msg -> Model -> Model
updateNPCs msg model =
    case msg of
        {-
        Frame _ ->
            Model.birdPosΔ model
            |> Model.lizardPosΔ
            |> Model.loverPosΔ
        -}
        _ -> model
