module Seeds exposing (..)

import Random
import Model exposing (Grass(..))
import Length exposing (Length)
import Angle exposing (Angle)

type alias MothFluttering = { xComponent : Length, yComponent : Length, θ: Angle }


bool : Random.Generator Bool
bool = 
    Random.uniform True [ False ]

grassKindSeed : Random.Generator Grass
grassKindSeed =
    Random.weighted (70, NormalGrass) [ (15, DenseGrass), (10, TallGrass), (4, FloweringGrass), (1, CloveredGrass) ]

grassFlipSeed : Random.Generator Bool
grassFlipSeed = bool

grassSeed : Random.Generator (Bool, Grass)
grassSeed =
    Random.pair grassFlipSeed grassKindSeed

grassAnimationSeed : Random.Generator Bool
grassAnimationSeed =
    Random.weighted (30, True) [ (70, False) ]

flutterMovements : Random.Generator MothFluttering
flutterMovements =
    Random.map3 
        (\x y θ -> 
            { xComponent = Length.centimeters x
            , yComponent = Length.centimeters y
            , θ = Angle.radians θ
            }
        )
        (Random.float 1 100)
        (Random.float 1 100)
        (Random.float 0 pi)

    
