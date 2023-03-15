module AntagonistTests exposing (suite)

import Angle
import Aviary.Birds exposing (goldfinch, robin, thrush)
import Axis2d
import Constants as C
import Direction2d
import Duration exposing (Duration)
import Expect
import Frame2d
import Fuzz exposing (Fuzzer)
import Helpers exposing (between, duple, flip, mapEach, uncurry, within)
import Length
import Model exposing (Model)
import Pixels
import Point2d
import QuadraticSpline2d
import Quantity
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test, todo)
import Vector2d
import Volume
import Aviary.Birds exposing (starling)
import Aviary.Birds exposing (cardinal)
import Constants exposing (Position)
import Constants exposing (Direction)
import Aviary.Birds exposing (kestrel)
import Util exposing (initModel, fuzzModel, withMothOnFlower, withSpiderWithinDistanceOfMoth, withSpiderPos, withBatInSwoopingPosition, withBatPos, iterateState, compareModelOver, withBatDir)
import Expect exposing (FloatingPointTolerance(..))




suite : Test
suite =
    [ fuzz (fuzzModel |> withMothOnFlower |> withSpiderWithinDistanceOfMoth)
        "Spider climbs flower and eats moth" <|
            \model ->
                let
                    tmodel = 
                        model
                        |> iterateState (Duration.seconds 0.1)
                        |> duple
                        |> Tuple.mapSecond (\m ->
                            let 
                                distance = Point2d.distanceFrom m.moth.position m.spider.position
                            in
                            iterateState (Quantity.at_ Constants.spiderPursuitSpeed distance) m
                        )
                in
                tmodel
                |> Expect.all
                    [ Tuple.first >> Model.gameOverか >> Expect.equal False >> Expect.onFail "Spider prematurely ended game"
                    , Tuple.second >> Model.gameOverか >> Expect.equal True >> Expect.onFail (Debug.toString (Tuple.second tmodel |> .spider) ++ "Spider didn't end game")
                    ]
    , fuzz2 (fuzzModel |> withMothOnFlower) (Fuzz.floatRange 1.0 5.0 |> Fuzz.map Duration.seconds)
        "Bat patrols skies" <|
            \model duration ->
                model
                |> compareModelOver (Quantity.plus (Duration.seconds 0.1) duration) (.bat >> .position) (Point2d.equalWithin (Quantity.at C.batFlightSpeed duration))
                    (Expect.equal False)

    , fuzz
        ( Fuzz.oneOfValues [(Direction2d.negativeX, C.minimalBatRange), (Direction2d.positiveX, C.maximalBatRange)] 
            |> Fuzz.map (Tuple.mapBoth Just (Point2d.along C.batFlightAxis >> Just))
            |> Fuzz.andThen (\(mbD, mbP) ->  (withBatDir mbD >> withBatPos mbP) (fuzzModel |> withMothOnFlower)))
        "Bat reverses direction eventually" <|
                ( compareModelOver 
                    Duration.second 
                    ((.bat >> duple >> Tuple.mapBoth .position .direction))
                    ((\(_, d1) (_, d2) -> Direction2d.angleFrom d1 d2))
                    (Angle.inDegrees
                        >> abs
                        >> Expect.within (Absolute 0.1) 180))
    , fuzz (fuzzModel |> withBatInSwoopingPosition)
        "Bat swoops and eats moth from 45° angle" <|
            \model ->
                let
                    distanceFromBatToMoth = Length.centimeters 0
                    timeToSwoop = Quantity.at_ Constants.batFlightSpeed distanceFromBatToMoth
                    tmodel =
                        model
                        |> iterateState (Duration.seconds 0.1)
                        |> duple
                        |> Tuple.mapSecond (\m ->
                            let 
                                distance = Point2d.distanceFrom m.moth.position m.bat.position
                            in
                            iterateState (Quantity.at_ Constants.batSwoopSpeed distance) m
                        )
                in
                tmodel
                |> Expect.all
                    [ Tuple.first >> Model.gameOverか >> Expect.equal False >> Expect.onFail "Bat prematurely ended game"
                    , Tuple.second >> Model.gameOverか >> Expect.equal True >> Expect.onFail (Debug.toString (Tuple.second tmodel |> .bat) ++ "Bat didn't end game")
                    ]
    , fuzz (fuzzModel |> withBatPos (Point2d.along C.batFlightAxis Length.meter |> Point2d.translateIn Direction2d.negativeY Length.meter |> Just))
        "Bat returns to flight axis if not attacking" <|
            ( compareModelOver 
                (Duration.seconds 1.1)
                (.bat >> .position) 
                (Point2d.equalWithin (Quantity.at C.batFlightSpeed Duration.second))
                (Expect.equal False))



    ]
        |> describe "Moth Tests"


