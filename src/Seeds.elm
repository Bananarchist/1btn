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
    Random.map5 
        (\flipX flipY x y θ -> 
            { xComponent = Length.centimeters (if flipX then x |> negate else x)
            , yComponent = Length.centimeters (if flipY then y |> negate else y)
            , θ = Angle.radians θ
            }
        )
        bool
        bool
        (Random.float 50 150)
        (Random.float 50 100)
        (Random.float (pi / 4) (pi + pi / 2))

    
