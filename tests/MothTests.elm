module MothTests exposing (suite)

import Angle
import Duration
import Expect
import Fuzz
import Helpers exposing (flip, uncurry)
import Length
import Model
import Pixels
import Point2d
import Quantity
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test, todo)


positiveFloat =
    Fuzz.floatAtLeast 0.0


fuzzDuration =
    Fuzz.map Duration.milliseconds


updateMothStatus : Duration.Duration -> Model.Model -> Model.Model
updateMothStatus tick =
    Model.setCurrentTick tick >> Model.updateMothStatus >> Model.setLastTick tick

fuzzStepper : Fuzz.Fuzzer Float -> Fuzz.Fuzzer (Float -> Model.Model -> Model.Model)
fuzzStepper =
    fuzzDuration
    >> Fuzz.map (\val -> \iter -> updateMothStatus (Quantity.multiplyBy iter val))

suite : Test
suite =
    moth

init = Model.init 600 400 0

step δ =
    \which -> updateMothStatus (Quantity.multiplyBy which δ)


moth : Test
moth =
    [ fuzz3 (Fuzz.floatRange 123.0 123456.0 |> fuzzStepper) positiveFloat positiveFloat "Moth angle clamped to 2pi" <|
        \stepStatus f1 f2 ->
            init
            |> stepStatus (f1)
            |> Model.setKeyboard True
            |> stepStatus (f1 + f2)
            |> Model.setKeyboard False
            |> stepStatus (f1 + f2 * 2)
            |> Model.setKeyboard True
            |> stepStatus (f1 * 2 + f2 * 2)
            |> Model.setKeyboard False
            |> Model.mothθ
            |> Debug.log "angle"
            |> Quantity.lessThanOrEqualTo (Angle.radians (2 * pi))
            |> Expect.equal True
    , fuzz (Fuzz.floatRange 1000.0 1000000.0) "Moth moves with time while idling" <|
        Duration.milliseconds
            >> flip updateMothStatus init
            >> Model.mothPos
            >> Expect.all
                [ Point2d.equalWithin
                    Length.centimeter
                    (Model.mothPos init)
                    >> Expect.equal False
                , Point2d.coordinates
                    >> Tuple.mapBoth Quantity.isNaN Quantity.isNaN
                    >> uncurry (&&)
                    >> Expect.equal False
                , Point2d.coordinates
                    >> Tuple.mapBoth Quantity.isInfinite Quantity.isNaN
                    >> uncurry (&&)
                    >> Expect.equal False
                ]
    , fuzz (Fuzz.floatRange 1000.0 10000.0 |> fuzzStepper) "Moth does not go below ground level" <|
        \stepStatus ->
            let
                model = init 
                    |> Model.setGrass [(True, Model.NormalGrass)]
                    |> Model.setKeyboard True
                    |> stepStatus 1
                    |> stepStatus 2
                    |> stepStatus 3
                    |> stepStatus 4
                    |> stepStatus 5
                    |> stepStatus 6
                    |> stepStatus 7
                    |> stepStatus 8
                    |> stepStatus 9
                    |> stepStatus 10
                mothYCoord = Model.mothCoords model |> Tuple.second
                mothAndGrassYCoords = Model.visibleGrassSegments model 
                    |> List.map .coords 
                    |> List.head 
                    |> Maybe.map (Tuple.mapFirst (always mothYCoord)) 
            in
            mothAndGrassYCoords
            |> Maybe.map (Debug.log "Moth and Grass coords" >> uncurry Quantity.greaterThan >> Expect.equal True)
            |> Maybe.withDefault (Expect.fail "Moth is below grass")
    , Test.only <| fuzz (Fuzz.floatRange 1000 100000 |> fuzzStepper) "Frame origin does not fall below ground level" <|
        \stepStatus ->
            let
                model = init 
                    |> Model.setKeyboard True
                    |> stepStatus 1
                    |> stepStatus 2
                    |> stepStatus 3
                    |> stepStatus 4
                    |> stepStatus 5
                    |> stepStatus 6
                    |> stepStatus 7
                    |> stepStatus 8
                    |> stepStatus 9
                    |> stepStatus 10
                frameYCoord = Model.framePos model 
                    |> Point2d.coordinates 
                    |> Tuple.second 
                    |> Quantity.plus (Length.centimeters 200)
                frameAndGrassYCoords = Model.visibleGrassSegments model 
                    |> List.map .coords 
                    |> List.head 
                    |> Maybe.map (Tuple.mapFirst (always frameYCoord)) 
            in
            frameAndGrassYCoords
            --|> Maybe.map (Debug.log "Moth and Frame coords" >> uncurry Quantity.greaterThan >> Expect.equal True)
            |> Maybe.map (uncurry Quantity.greaterThan >> Expect.equal True)
            |> Maybe.withDefault (Expect.fail "Moth is below grass")
    , Test.only <| fuzz (Fuzz.floatRange 200 450 |> fuzzStepper) "3KP produces 3 Farts" <|
        \stepStatus ->
            let 
                model = 
                    init
                    |> Model.setKeyboard True
                    |> stepStatus 1
                    |> stepStatus 2
                    |> Model.setKeyboard False
                    |> stepStatus 3
                    |> Model.setKeyboard True
                    |> stepStatus 4
                    |> Model.setKeyboard False
                    |> stepStatus 5
                    |> Model.setKeyboard True
                    |> stepStatus 6
                thenModel =
                    Model.setKeyboard False model
                    |> stepStatus 500000
                fartTests = 
                    (Model.farts model, Model.farts thenModel)
                failString = Debug.toString model
            in
            fartTests
            |> Tuple.mapBoth List.length List.length
            |> Expect.all 
                [ uncurry Expect.notEqual >> Expect.onFail (fartTests |> Debug.toString)
                , Tuple.first >> Expect.equal 3 >> Expect.onFail failString
                ]
    , fuzz ((Fuzz.floatRange 1000.0 10000.0) |> fuzzStepper) "If keys are pressed, gas depletes" <|
        \stepStatus ->
            init
                |> Model.setKeyboard True
                |> stepStatus 1
                |> Debug.log "Moth"
                |> stepStatus 2
                |> Debug.log "Moth again"
                |> Model.gas
                |> Expect.lessThan (Model.gas init)
    , test "Gas should never fall below 0" <|
        \_ ->
            init
                |> Model.setKeyboard True
                |> updateMothStatus (Duration.milliseconds 1000000)
                |> Model.gas
                |> Expect.all
                    [ Expect.atLeast 0
                    , Expect.lessThan 100
                    ]
    , fuzz2 Fuzz.bool positiveFloat "Keys pressed and nonzero gas means moth is moving" <|
        \kbか gas ->
            let
                updated =
                    init |> Model.setKeyboard kbか |> updateMothStatus (Duration.milliseconds (gas * 1000))
            in
            Expect.equal (Model.mothFartingか updated) (kbか && Model.gas updated > 0)
    , fuzz (fuzzDuration (Fuzz.floatRange 1000.0 10000.0)) "Moth changes angles while idling" <|
        \δ ->
            let
                firstGo = init
                    |> updateMothStatus δ
            in
            Quantity.equalWithin 
                (Angle.radians (pi / 4)) 
                (firstGo |> Model.mothθ)
                (firstGo |> updateMothStatus (Quantity.plus δ (Duration.milliseconds 1500)) |> Model.mothθ)
            |> Expect.equal False
    , test "Moth feeds when near flowers with < 100% gas" <|
        \_ ->
            let
                model =
                    init
                        |> Model.setFlowers [ ( Model.Flower, 20 ) ]
                        |> Model.setKeyboard True
                        |> updateMothStatus (Duration.milliseconds 1)
                        |> updateMothStatus (Duration.milliseconds 1500)
                        |> Model.setKeyboard False
                        |> updateMothStatus (Duration.milliseconds 1501)
                        |> updateMothStatus (Duration.milliseconds 2501)
            in
            Expect.equal (Model.mothFeedingか model) True
    , test "Resting on flowers replenishes gas" <|
        \_ ->
            let
                preFeed =
                    init
                        |> Model.setFlowers [ ( Model.Flower, 20 ) ]
                        |> Model.setKeyboard True
                        |> updateMothStatus (Duration.milliseconds 1000)
                        |> Model.setKeyboard False
                        |> updateMothStatus (Duration.milliseconds 1100)

                postFeed =
                    preFeed
                        |> updateMothStatus (Duration.milliseconds 1200)
            in
            Expect.greaterThan (Model.gas preFeed) (Model.gas postFeed)
    , test "Moth reaching lover == victory" <|
        \_ ->
            init
                |> Model.setLover ( 20, 20 )
                |> Model.setKeyboard True
                |> updateMothStatus (Duration.milliseconds 500)
                |> updateMothStatus (Duration.milliseconds 501)
                |> Model.setKeyboard False
                |> updateMothStatus (Duration.milliseconds 1500)
                |> updateMothStatus (Duration.milliseconds 1501)
                |> Expect.all
                    [ Model.gameOverか >> Expect.equal True
                    ]
    , todo "Animation states for moth landing"
    , todo "Fart lines"
    , todo "Moth staying too low too long == lizard loss"
    , todo "Moth staying too high too long == sun loss"
    , todo "Moth idling in one spot too long == bird loss"
    , todo "Won't land if gas is 100"
    , todo "Cutscene motion?"
    ]
        |> describe "Moth tests"
