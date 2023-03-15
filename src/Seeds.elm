module Seeds exposing (..)

import Random
import Model exposing (MothFluttering)
import Helpers exposing (mapEach, uncurry)
import Length exposing (Length)
import Axis2d
import Angle exposing (Angle)
import Point2d
import Frame2d
import Constants exposing (Position, QuadraticSpline)
import QuadraticSpline2d


bool : Random.Generator Bool
bool = 
    Random.uniform True [ False ]

{-
grassKindSeed : Random.Generator Grass
grassKindSeed =
    Random.weighted (70, NormalGrass) [ (15, DenseGrass), (10, TallGrass), (4, FloweringGrass), (1, CloveredGrass) ]
-}

grassFlipSeed : Random.Generator Bool
grassFlipSeed = bool

{-
grassSeed : Random.Generator (Bool, Grass)
grassSeed =
    Random.pair grassFlipSeed grassKindSeed
-}

grassAnimationSeed : Random.Generator Bool
grassAnimationSeed =
    Random.weighted (30, True) [ (70, False) ]


flutterMovements : Random.Generator QuadraticSpline
flutterMovements =
    Random.map5
        (\endPoint x2 y2 arcBack arcDown ->
            let
                yMod = if arcDown then Point2d.mirrorAcross Axis2d.y else identity
                xMod = if arcBack then Point2d.mirrorAcross Axis2d.x else identity
            in 
            QuadraticSpline2d.fromControlPoints
                Point2d.origin
                (Point2d.centimeters x2 y2 |> yMod |> xMod) --|> Point2d.placeIn (Frame2d.atPoint endPoint))
                (endPoint |> yMod |> xMod)
        )
        randomPositivePosition
        (Random.float 10 80)
        (Random.float 100 200)
        bool
        bool


randomPosition : Random.Generator Position
randomPosition =
    Random.map4
        (\x y nx ny -> 
            Point2d.centimeters (if nx then negate x else x) (if ny then negate y else y)
        )
        (Random.float 90 200)
        (Random.float 90 200)
        bool
        bool

randomPositivePosition : Random.Generator Position
randomPositivePosition =
    Random.map2
        Point2d.centimeters
        (Random.float 90 200)
        (Random.float 0 100)
