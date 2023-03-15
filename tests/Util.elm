module Util exposing (..)

import Expect
import Model exposing (Model)
import Fuzz exposing (Fuzzer)
import Direction2d
import Point2d
import Constants as C exposing (Position, Direction)
import Volume
import Length
import Helpers exposing (flip, duple, uncurry, mapEach)
import Quantity
import QuadraticSpline2d
import Vector2d
import Aviary.Birds exposing (kestrel, robin)
import Duration exposing (Duration)

initModel =
    Model.init 600 400 0.0


initMoth fn =
    let
        m =
            initModel.moth
    in
    { initModel | moth = fn m }


{-| Iterate model over duration, run comparison of mapped versions of both initial and iterated model, and if expectation
fails, print out the mapped versions. -}
compareModelOver : Duration -> (Model -> b) -> (b -> b -> d) -> (d -> Expect.Expectation) -> Model -> Expect.Expectation
compareModelOver duration mapper comparator expectation =
    duple
    >> Tuple.mapSecond (iterateState duration)
    >> mapEach mapper
    >> duple
    >> Tuple.mapBoth Debug.toString (uncurry comparator >> expectation)
    >> uncurry Expect.onFail


fuzzModel =
    Fuzz.constant initModel


withLoverPos mbPos fuzzedModel =
    mbPos
        |> Maybe.map Fuzz.constant
        |> Maybe.withDefault fuzzPoint2d
        |> Fuzz.map2 (\fm p -> (\lover -> { fm | lover = { lover | position = p } }) fm.lover) fuzzedModel

withSpiderPos mbPos fuzzedModel =
    mbPos
        |> Maybe.map Fuzz.constant
        |> Maybe.withDefault fuzzPoint2d
        |> Fuzz.map2 (\fm p -> (\spider -> { fm | spider = { spider | position = p, pursuing = Nothing } }) fm.spider) fuzzedModel
    
withSpiderWithinDistanceOfMoth fuzzedModel =
    Fuzz.andThen 
        (\fm -> 
            withSpiderPos 
            (Just <| Point2d.xy (Point2d.xCoordinate fm.moth.position) (Length.centimeters 0.0)) 
            (Fuzz.constant fm))
        fuzzedModel

withBatPos mbPos fuzzedModel =
    mbPos
        |> Maybe.map Fuzz.constant
        |> Maybe.withDefault fuzzPoint2d
        |> Fuzz.map2 (\fm p -> (\bat -> { fm | bat = { bat | position = p } }) fm.bat) fuzzedModel

withBatDir mbDir fuzzedModel =
    mbDir
        |> Maybe.map Fuzz.constant
        |> Maybe.withDefault fuzzDirection
        |> Fuzz.map2 (\fm d -> (\bat -> { fm | bat = { bat | direction = d } }) fm.bat) fuzzedModel
    
    
withBatInSwoopingPosition fuzzedModel =
    Fuzz.andThen 
        (\fm -> 
            withBatPos 
            (Just <| Point2d.translateBy 
                (Vector2d.withLength (Point2d.signedDistanceFrom C.batFlightAxis fm.moth.position |> duple |> uncurry Quantity.product |> Quantity.twice |> Quantity.sqrt) (Direction2d.degrees 45)) 
                fm.moth.position) 
            (Fuzz.constant fm)
            |> withBatDir (Just <| Direction2d.negativeX))
        fuzzedModel


withMovements mbMoves fuzzedModel =
    mbMoves
        |> Maybe.map Fuzz.constant
        |> Maybe.withDefault (Fuzz.listOfLengthBetween 1 5 fuzzQuadSpline)
        |> Fuzz.map2 (flip Model.setIdleMovements) fuzzedModel


withMappedMoth : Maybe (Model -> Model) -> Fuzzer Model -> Fuzzer Model
withMappedMoth mbMothFn fuzzedModel =
    mbMothFn
        |> Maybe.map (robin fuzzedModel Fuzz.map)
        |> Maybe.withDefault fuzzedModel

withMothPosition : Position -> Fuzzer Model -> Fuzzer Model
withMothPosition pos =
    withMappedMoth (Just <| (\model -> (\moth -> { model | moth = { moth | position = pos }}) model.moth))

withMothDirection : Direction -> Fuzzer Model -> Fuzzer Model
withMothDirection dir =
    withMappedMoth (Just <| (\model -> (\moth -> { model | moth = { moth | direction = dir }}) model.moth))

withMothOnFlower : Fuzzer Model -> Fuzzer Model
withMothOnFlower =
    withMappedMoth (Just <| \model ->
        let
            ( flowerKind, flowerPos ) =
                List.foldl kestrel (Model.Normal, Point2d.origin) model.food.flowers
            moth = model.moth
        in
        { model
        | moth =
            { moth
            | position = Point2d.translateIn Direction2d.positiveY (Model.flowerHeight flowerKind) flowerPos
            , gas = Volume.milliliters 50
            } 
        }
        |> Model.update
    )


updateMothState newTick =
    Model.setCurrentTick newTick
        >> Model.update
        >> Model.setLastTick newTick


iterateState elapsed =
    duple
        >> Tuple.mapFirst (.system >> .lastTick >> Quantity.plus elapsed)
        >> uncurry updateMothState


fuzzPoint2d =
    Fuzz.map2 Point2d.centimeters (Fuzz.floatRange 10.0 100.0) (Fuzz.floatRange 10.0 100.0)


fuzzQuadSpline =
    Fuzz.map2 Tuple.pair fuzzPoint2d fuzzPoint2d
        |> Fuzz.andThen
            (\( sp, ep ) ->
                let
                    rep =
                        if Point2d.equalWithin (Length.centimeters 0.1) sp ep then
                            Point2d.translateIn Direction2d.x (Length.centimeters 50) ep

                        else
                            ep
                in
                Fuzz.map3 QuadraticSpline2d.fromControlPoints (Fuzz.constant sp) fuzzPoint2d (Fuzz.constant rep)
            )


fuzzDirection =
    Fuzz.map Direction2d.degrees Fuzz.niceFloat

