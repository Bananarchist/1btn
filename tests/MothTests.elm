module MothTests exposing (suite)

import Angle
import Aviary.Birds exposing (goldfinch, robin, thrush)
import Axis2d
import Constants
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
import Util exposing (initModel, withLoverPos, withMappedMoth, withMothDirection, withMothOnFlower, withMothPosition, withMovements, iterateState, fuzzModel, initMoth, fuzzDirection)

suite : Test
suite =
    [ test "Key depressed (assuming moth starting gas > 0) puts moth into farting state" <|
        \_ ->
            initModel
                |> Model.setKeyboard True
                |> Model.update
                |> Model.mothFartingか
                |> Expect.equal True
    , fuzz (fuzzModel |> withMovements Nothing) "While idle, moth moves about" <|
        \model ->
            let
                (ogPos, newPos) =
                    model
                    |> duple
                    |> Tuple.mapSecond
                        ( iterateState (Duration.seconds 0.6) >> iterateState (Duration.seconds 1.0) )
                    |> mapEach (Model.viewData >> .moth >> .position)
            in
            newPos
                |> Point2d.equalWithin (Pixels.pixels 0.1) ogPos
                |> Expect.equal False
                |> Expect.onFail (Debug.toString ogPos ++ Debug.toString newPos)
    , test "Moth loses gas while moving, stops moving when out of gas" <|
        \_ ->
            let
                overUseTime =
                    Quantity.at_ Constants.gasPerSecondOfFarting (Volume.milliliters 120.0)
            in
            initModel
                |> Model.setKeyboard True
                |> iterateState (Duration.seconds 0.1)
                |> iterateState overUseTime
                |> iterateState (Duration.seconds 0.1)
                |> Model.mothFartingか
                |> Expect.equal False
    , fuzz 
        ( fuzzModel 
            |> withLoverPos (Just <| Point2d.centimeters 100 50) 
            |> withMothPosition (Point2d.centimeters 50 50)
            |> withMothDirection Direction2d.x)
        "When moth reaches lover, game is over" <|
        \model ->
            let
                travelTime =
                    Quantity.at_ Constants.metersPerSecondOfFarting (Length.centimeters 50)
            in
            model
                |> Model.setKeyboard True
                |> Model.update
                |> iterateState travelTime
                |> Model.gameOverか
                |> Expect.equal True
                |> Expect.onFail (Debug.toString model.moth.position ++ ", " ++ Debug.toString model.lover.position)
    , fuzz 
        (fuzzModel |> withMothPosition (Point2d.centimeters 100 1000)) 
        "Camera follows moth when farting" <|
        \initialModel ->
            let
                model =
                    initialModel
                        |> Model.setKeyboard True
                        |> Model.update
                        |> iterateState Duration.second
                        |> duple
                        |> Tuple.mapSecond (iterateState Duration.second)

                frameDistanceTraveled =
                    model
                    |> mapEach (.frame >> Frame2d.originPoint >> Point2d.at Constants.pixelsPerCentimeter)
                    |> uncurry Point2d.distanceFrom

                mothDistanceTraveled =
                    model
                    |> mapEach (.moth >> .position)
                    |> uncurry Point2d.distanceFrom

            in
            Quantity.equalWithin (Length.centimeters 0.1) frameDistanceTraveled mothDistanceTraveled
                |> Expect.equal True
                |> Expect.onFail (Debug.toString frameDistanceTraveled ++ ", " ++ Debug.toString mothDistanceTraveled)
    , fuzz fuzzDirection "Moth never flies upside down" <|
        \direction ->
            let
                model =
                    initMoth (\m -> { m | direction = direction })
                        |> Model.update

                shouldMirror =
                    Direction2d.xComponent direction < 0

                angle =
                    Direction2d.toAngle direction

                expectedAngle =
                    case ( shouldMirror, Direction2d.yComponent direction < 0 ) of
                        ( True, True ) ->
                            Quantity.plus angle (Angle.degrees 180) |> Angle.inDegrees

                        ( True, False ) ->
                            Quantity.minus angle (Angle.degrees 180) |> Angle.inDegrees

                        ( False, _ ) ->
                            angle |> Quantity.negate |> Angle.inDegrees

                viewData =
                    Model.viewData model
            in
            viewData
                |> Expect.all
                    [ .moth >> .mirrorX >> Expect.equal shouldMirror
                    , .moth >> .rotation >> Angle.inDegrees >> Expect.within (Expect.Absolute 0.1) expectedAngle
                    ]
                |> Expect.onFail
                    (String.join " "
                        [ "ShouldMirror:"
                        , Debug.toString shouldMirror
                        , "and expectedAngle:"
                        , Debug.toString expectedAngle
                        , "But received mirrorX:"
                        , Debug.toString viewData.moth.mirrorX
                        , "and angle:"
                        , Debug.toString (viewData.moth.rotation |> Angle.inDegrees)
                        ]
                    )
    , fuzz 
        (fuzzModel 
            |> withMothPosition (Point2d.centimeters 50 75)
            |> withMothDirection (Direction2d.degrees -90))
        "Moth may not go below ground" <|
        \model ->
            model
                |> Model.setKeyboard True
                |> Model.update
                |> iterateState Duration.millisecond
                |> .moth
                |> .position
                |> Point2d.translateIn Direction2d.negativeY (Quantity.half Constants.mothWidth)
                |> Point2d.signedDistanceFrom Axis2d.x
                |> Length.inCentimeters
                |> Expect.greaterThan (Length.inCentimeters Constants.groundLevel)
    , fuzz (fuzzModel |> withMothOnFlower )
        "When moth is feeding, gas replenishes" <|
        \model ->
            let
                initialGas =
                    model |> Model.viewData |> .gas
            in
            model
            |> iterateState (Duration.seconds 0.5)
            |> iterateState (Duration.seconds 0.5)
            |> Model.viewData
            |> .gas
            |> Expect.greaterThan initialGas
    , fuzz 
        (fuzzModel
            |> withMappedMoth (Just <|
                \model ->
                    let
                        ( flowerKind, flowerPos ) =
                            List.foldl kestrel (Model.Normal, Point2d.origin) model.food.flowers
                        moth = model.moth
                    in
                    { model
                    | moth =
                        { moth
                        | position = Point2d.translateIn Direction2d.positiveY (Model.flowerHeight flowerKind) flowerPos
                            |> Point2d.translateIn Direction2d.negativeX (Length.centimeters 100)
                        , direction = Direction2d.positiveX
                        , gas = Volume.milliliters 99
                        } 
                    }
                    |> Model.setKeyboard True
                    |> Model.update
                    |> iterateState (Duration.seconds 0.4)
                    |> Model.update
                    
                )
        )
        "When moth is feeding, {new} input causes takeoff" <|
            \model ->
                let
                    expectFeeding =
                        model |> Model.setKeyboard False |> Model.update
                    expectEscapingFromFeeding =
                        expectFeeding |> Model.setKeyboard True |> Model.update 
                    expectSitting = 
                        expectFeeding |> iterateState (Duration.seconds 0.5) 
                    expectEscapingFromSitting =
                        expectSitting |> Model.setKeyboard True |> Model.update
                    expectIdling =
                        expectEscapingFromSitting |> Model.setKeyboard False |> Model.update |> iterateState (Duration.seconds 1.5) |> Model.update
                in
                Expect.all
                    [ kestrel expectFeeding >> Model.mothFeedingか >> Expect.equal True >> Expect.onFail "Moth not feeding"
                    , kestrel expectEscapingFromFeeding >> Model.mothEscapingか >> Expect.equal True >> Expect.onFail "Moth not escaping from feeding"
                    , kestrel expectSitting >> Model.mothSittingか >> Expect.equal True >> Expect.onFail "Moth not sitting"
                    , kestrel expectEscapingFromSitting >> Model.mothEscapingか >> Expect.equal True >> Expect.onFail "Moth not escaping from sitting"
                    , kestrel expectIdling >> Model.mothIdlingか >> Expect.equal True >> Expect.onFail "Moth not idling"
                    , kestrel expectIdling >> .moth >> .position >> Point2d.yCoordinate >> Quantity.greaterThan (expectSitting.moth.position |> Point2d.yCoordinate) >> Expect.equal True
                    ] model
    ]
        |> describe "Moth Tests"


