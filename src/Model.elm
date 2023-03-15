module Model exposing
    ( update
    , Model, ViewData, EntityViewData, init
    , gameOverか, viewData
    , setPad, setKeyboard, setMouse, setListeningPad, setListeningControls, anyPressedか, listeningか, padCount, justPressedか, justReleasedか, pressedFor
    , setLastTick, setCurrentTick
    , setWindow, anyNotificationsか
    , MothFluttering, State(..)
    , mothFartingか, mothFeedingか, mothPendingMovementsか, mothIdlingか, mothSittingか, mothEscapingか
    , setIdleMovements
    , Flower(..)
    , flowerHeight, flowerLandingYComponent
    )

{-| Model and API


# High Level

@docs update


## Types

@docs Model, ViewData, EntityViewData, init


## Game State

@docs gameOverか, viewData


# System

This type and assorted functions are for managing controls, time and the browser


## Controls

@docs setPad, setKeyboard, setMouse, setListeningPad, setListeningControls, anyPressedか, listeningか, padCount, anyPressedか, justPressedか, justReleasedか, pressedFor


## Time

@docs setLastTick, setCurrentTick, tickδ


## Output

@docs setWindow, anyNotificationsか


# Moth


## Types

@docs MothFluttering, State


## Getters

@docs mothFartingか, mothFeedingか, mothPendingMovementsか, mothIdlingか, mothSittingか, mothEscapingか


## Setters

@docs setIdleMovements


# Food


## Types

@docs Flower


## Getters

@docs flowerHeight, flowerLandingYComponent

-}

import Angle exposing (Angle)
import Area
import Aviary.Birds exposing (cardinal, finchStar, kestrel, phoenix, starling)
import Axis2d
import BoundingBox2d
import Constants as C exposing (Direction, Distance, Frame, Gas, HitBox, Position, PursuitPath, QuadraticSpline, ScreenPosition, WalkingPath, gasPerSecondOfFarting, metersPerSecondIdling, metersPerSecondOfFarting, mothHeight, mothWidth, pixelsPerCentimeter)
import Direction2d
import Duration exposing (Duration)
import Frame2d
import Helpers exposing (allPredicates, anyPredicates, appendIf, between, buildl, curry, defaultTo, duple, dupleInto, flip, liftTuple, mapEach, nearest, uncurry, within)
import Length
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Maybe.Extra
import Parameter1d
import Pixels exposing (Pixels)
import Point2d
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity)
import Queue as Q
import Vector2d
import Volume


type alias Model =
    { system : System
    , moth : Moth
    , frame : Frame
    , lover : Moth
    , phase : Phase
    , food : Food
    , spider : Antagonist
    , bat : Antagonist
    }


type alias EntityViewData =
    { position : ScreenPosition
    , mirrorX : Bool
    , rotation : Angle
    }


type alias ViewData =
    { moth : EntityViewData
    , lover : EntityViewData
    , gas : Float
    , grass : List { coordinates : ScreenPosition }
    , flowers : List { coordinates : ScreenPosition, kind : Flower }
    }


init : Int -> Int -> Float -> Model
init width height currentTick =
    let
        moth =
            mothInit

        model =
            { system = systemInit width height currentTick
            , moth = moth
            , frame = Frame2d.atOrigin |> Frame2d.reverseY
            , lover = { moth | position = Point2d.translateIn Direction2d.positiveX (Length.centimeters 500) moth.position }
            , phase = Playing
            , food = initFood
            , spider = initSpider
            , bat = initBat
            }
    in
    centerCameraOn model.moth.position model


update : Model -> Model
update model =
    let
        tδ =
            tickδ model.system

        mothStateUpdated =
            model
                |> mapMoth
                    (updateState
                        { elapsedTime = tδ
                        , isPressed = anyPressedか model
                        , controlStateJustChanged = justPressedか model || justReleasedか model
                        }
                    )
                |> landOnNearbyFlower
                |> updateFlowerIndex
                |> updateSpider
                |> updateBat

        gamePhaseUpdated =
            updateGamePhase mothStateUpdated
    in
    gamePhaseUpdated
        |> updateCamera model
        |> updateNotifications


viewData : Model -> ViewData
viewData model =
    { moth =
        let
            angle =
                Direction2d.toAngle model.moth.direction

            ( xLtz, yLtz ) =
                Direction2d.components model.moth.direction |> mapEach ((>) 0)
        in
        { position = model.moth.position |> Point2d.at_ pixelsPerCentimeter |> Point2d.relativeTo model.frame
        , mirrorX = xLtz
        , rotation =
            case ( xLtz, yLtz ) of
                ( True, True ) ->
                    Quantity.plus angle (Angle.degrees 180)

                ( True, False ) ->
                    Quantity.minus angle (Angle.degrees 180)

                ( _, _ ) ->
                    Quantity.negate angle
        }
    , lover =
        { position = model.lover.position |> Point2d.at_ pixelsPerCentimeter |> Point2d.relativeTo model.frame
        , mirrorX = False
        , rotation = Angle.degrees 0
        }
    , gas = Volume.inMilliliters model.moth.gas
    , grass =
        List.range -75 75
            |> List.map
                ((*) 32
                    >> toFloat
                    >> flip Point2d.centimeters 0.0
                    >> Point2d.at_ pixelsPerCentimeter
                    >> Point2d.relativeTo model.frame
                    >> (\p -> { coordinates = p })
                )
    , flowers = List.map (\( f, p ) -> { coordinates = p |> Point2d.at_ pixelsPerCentimeter |> Point2d.relativeTo model.frame, kind = f }) model.food.flowers
    }


mapSystem : (System -> System) -> Model -> Model
mapSystem fn model =
    { model | system = fn model.system }


mapMoth : (Moth -> Moth) -> Model -> Model
mapMoth fn model =
    { model | moth = fn model.moth }


mapFood : (Food -> Food) -> Model -> Model
mapFood fn model =
    { model | food = fn model.food }


mapSpider : (Antagonist -> Antagonist) -> Model -> Model
mapSpider fn model =
    { model | spider = fn model.spider }


mapBat : (Antagonist -> Antagonist) -> Model -> Model
mapBat fn model =
    { model | bat = fn model.bat }


{-| System
-}
type alias System =
    { controls : Controls
    , lastTick : Duration
    , currentTick : Duration
    , width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , notifications : Q.Queue ( Duration, String )
    }


type ControlState
    = PressedAt Duration
    | ReleasedAt Duration
    | Inactive


type alias Controls =
    { kb : ControlState
    , mouse : ControlState
    , gp : ControlState
    , input : Bool
    , padCount : Int
    }


systemInit : Int -> Int -> Float -> System
systemInit width height currentTick =
    { controls =
        { kb = Inactive
        , mouse = Inactive
        , gp = Inactive
        , input = True
        , padCount = 0
        }
    , lastTick = Duration.seconds currentTick
    , currentTick = Duration.seconds currentTick
    , width = Pixels.int width
    , height = Pixels.int height
    , notifications = []
    }


{-| Time
-}
tickδ : System -> Duration
tickδ { currentTick, lastTick } =
    Quantity.difference currentTick lastTick


setLastTick : Duration -> Model -> Model
setLastTick t =
    mapSystem (\m -> { m | lastTick = t })


setCurrentTick : Duration -> Model -> Model
setCurrentTick t =
    mapSystem (\m -> { m | currentTick = t })


{-| Controls
-}
anyPressedか : Model -> Bool
anyPressedか model =
    if listeningか model then
        model.system.controls
            |> anyPredicates (List.map ((<<) inPressedStateか) [ .kb, .mouse, .gp ])

    else
        False


inPressedStateか : ControlState -> Bool
inPressedStateか cs =
    case cs of
        PressedAt _ ->
            True

        _ ->
            False


listeningか : Model -> Bool
listeningか =
    .system >> .controls >> .input


justReleasedか : Model -> Bool
justReleasedか model =
    case controlsState model of
        ReleasedAt x ->
            Quantity.equalWithin Duration.millisecond x model.system.currentTick

        _ ->
            False


justPressedか : Model -> Bool
justPressedか model =
    case controlsState model of
        PressedAt x ->
            Quantity.equalWithin Duration.millisecond x model.system.currentTick

        _ ->
            False


pressedFor : Model -> Duration
pressedFor model =
    case controlsState model of
        PressedAt x ->
            Quantity.difference x model.system.currentTick

        _ ->
            Duration.seconds 0


{-| This is a definite source of bugs and needs testing
-}
controlsState : Model -> ControlState
controlsState =
    .system
        >> .controls
        >> buildl (List.map ((<<) List.singleton) [ .kb, .mouse, .gp ])
        >> List.foldl
            (\state acc ->
                case ( state, acc ) of
                    ( ReleasedAt s, ReleasedAt a ) ->
                        ReleasedAt (Quantity.max s a)

                    ( PressedAt s, _ ) ->
                        PressedAt s

                    ( _, PressedAt a ) ->
                        PressedAt a

                    ( ReleasedAt s, _ ) ->
                        ReleasedAt s

                    ( _, ReleasedAt a ) ->
                        ReleasedAt a

                    ( _, _ ) ->
                        state
            )
            Inactive


padCount : Model -> Int
padCount =
    .system
        >> .controls
        >> .padCount


mapControls : (Controls -> Controls) -> System -> System
mapControls fn system =
    { system | controls = fn system.controls }


setPadCount : Int -> Model -> Model
setPadCount count =
    mapSystem (mapControls (\c -> { c | padCount = count }))


pressControls : ControlState -> Duration -> ControlState
pressControls state =
    case state of
        Inactive ->
            PressedAt

        ReleasedAt _ ->
            PressedAt

        _ ->
            kestrel state


releaseControls : ControlState -> Duration -> ControlState
releaseControls state =
    case state of
        PressedAt _ ->
            ReleasedAt

        _ ->
            kestrel Inactive


pressKeyboard : Model -> Model
pressKeyboard =
    mapSystem (\sys -> mapControls (\c -> { c | kb = pressControls c.kb sys.currentTick }) sys)


depressKeyboard : Model -> Model
depressKeyboard =
    mapSystem (\sys -> mapControls (\c -> { c | kb = releaseControls c.kb sys.currentTick }) sys)


setKeyboard : Bool -> Model -> Model
setKeyboard state =
    if state then
        pressKeyboard

    else
        depressKeyboard


pressMouse : Model -> Model
pressMouse =
    mapSystem (\sys -> mapControls (\c -> { c | mouse = pressControls c.mouse sys.currentTick }) sys)


depressMouse : Model -> Model
depressMouse =
    mapSystem (\sys -> mapControls (\c -> { c | mouse = releaseControls c.mouse sys.currentTick }) sys)


setMouse : Bool -> Model -> Model
setMouse state =
    if state then
        pressMouse

    else
        depressMouse


pressPad : Model -> Model
pressPad =
    mapSystem (\sys -> mapControls (\c -> { c | gp = pressControls c.gp sys.currentTick }) sys)


depressPad : Model -> Model
depressPad =
    mapSystem (\sys -> mapControls (\c -> { c | gp = releaseControls c.gp sys.currentTick }) sys)


setPad : Bool -> Model -> Model
setPad state =
    if state then
        pressPad

    else
        depressPad


setListeningPad : Bool -> Model -> Model
setListeningPad state =
    if state then
        incrPadCount
            >> addNotification "Game pad connected"

    else
        decrPadCount
            >> addNotification "Game pad disconnected"


setListeningControls : Bool -> Model -> Model
setListeningControls bool =
    mapSystem (mapControls (\c -> { c | input = bool }))


incrPadCount : Model -> Model
incrPadCount =
    mapSystem (mapControls (\c -> { c | padCount = c.padCount + 1 }))


decrPadCount : Model -> Model
decrPadCount =
    mapSystem (mapControls (\c -> { c | padCount = max 0 (c.padCount - 1) }))


{-| Notifications
-}
addNotification : String -> Model -> Model
addNotification notification =
    mapSystem (\s -> { s | notifications = Q.enqueue ( Constants.notificationDuration, notification ) s.notifications })


updateNotifications : Model -> Model
updateNotifications model =
    case model.system.notifications of
        ( nd, nmsg ) :: ns ->
            if Quantity.greaterThanZero nd then
                mapSystem (\m -> { m | notifications = ( Quantity.minus (tickδ model.system) nd, nmsg ) :: ns }) model

            else
                mapSystem (\m -> { m | notifications = ns }) model

        _ ->
            model


anyNotificationsか : Model -> Bool
anyNotificationsか =
    .system >> .notifications >> Q.enqueued


{-| Camera
-}
centerCameraOn : Position -> Model -> Model
centerCameraOn pos model =
    let
        halfScreenX =
            model.system.width |> Quantity.toFloatQuantity |> Quantity.half

        halfScreenY =
            model.system.height |> Quantity.toFloatQuantity |> Quantity.half
    in
    { model
        | frame =
            Frame2d.moveTo (Point2d.at_ pixelsPerCentimeter pos) model.frame
                |> Frame2d.translateIn Direction2d.negativeX halfScreenX
                |> Frame2d.translateIn Direction2d.positiveY halfScreenY
    }


updateCamera : Model -> Model -> Model
updateCamera oldModel newModel =
    if not <| mothIdlingか newModel then
        centerCameraOn newModel.moth.position newModel

    else
        newModel


{-| Map
-}
groundLevelX =
    0.0


{-| Output
-}
setWindow : Quantity Int Pixels -> Quantity Int Pixels -> Model -> Model
setWindow newWidth newHeight =
    mapSystem (\m -> { m | width = newWidth, height = newHeight })


{-| Game flow
-}
type Phase
    = Playing
    | GameOver


gameOverか : Model -> Bool
gameOverか =
    .phase >> (==) GameOver


reachedLoverか : Model -> Bool
reachedLoverか =
    phoenix
        (Point2d.equalWithin (Length.centimeters 50))
        (.moth >> .position)
        (.lover >> .position)


caughtBySpiderか : Model -> Bool
caughtBySpiderか =
    phoenix
        (Point2d.equalWithin (Length.centimeters 10))
        (.moth >> .position)
        (.spider >> .position)


caughtByBatか : Model -> Bool
caughtByBatか =
    phoenix
        (Point2d.equalWithin (Length.centimeters 10))
        (.moth >> .position)
        (.bat >> .position)


updateGamePhase : Model -> Model
updateGamePhase model =
    case model.phase of
        Playing ->
            if reachedLoverか model then
                { model | phase = GameOver }

            else if caughtBySpiderか model then
                { model | phase = GameOver }

            else if caughtByBatか model then
                { model | phase = GameOver }

            else
                model

        _ ->
            model


{-| Moth
-}
type alias Moth =
    { position : Position
    , direction : Direction
    , state : State
    , gas : Gas
    , flutterings : Q.Queue QuadraticSpline --MothFluttering
    , next : List ( Position, Direction, Duration )
    }


type State
    = Idle
    | Farting -- Duration
    | Feeding
    | Sitting
    | Escaping Distance


startEscaping =
    Escaping (Length.centimeters 150)


type alias MothFluttering =
    { endPoint : Position
    , controlPoint : Position
    , arcBack : Bool
    , arcDown : Bool
    }


mothStateTest : State -> Model -> Bool
mothStateTest testState model =
    case ( testState, model.moth.state ) of
        ( Farting, Farting ) ->
            True

        ( Feeding, Feeding ) ->
            True

        ( Sitting, Sitting ) ->
            True

        ( Escaping _, Escaping _ ) ->
            True

        ( Idle, Idle ) ->
            True

        ( _, _ ) ->
            False


mothFartingか : Model -> Bool
mothFartingか =
    mothStateTest Farting


mothFeedingか : Model -> Bool
mothFeedingか =
    mothStateTest Feeding


mothEscapingか : Model -> Bool
mothEscapingか =
    mothStateTest startEscaping


mothSittingか : Model -> Bool
mothSittingか =
    mothStateTest Sitting


mothIdlingか : Model -> Bool
mothIdlingか =
    mothStateTest Idle


mothHitBox : Moth -> HitBox
mothHitBox { position } =
    BoundingBox2d.withDimensions
        ( Constants.mothWidth, Constants.mothHeight )
        position


mothPendingMovementsか : Model -> Bool
mothPendingMovementsか =
    .moth >> .flutterings >> Q.enqueued


mothInit : Moth
mothInit =
    { position = Point2d.centimeters 0 300
    , direction = Direction2d.positiveX
    , state = Idle
    , gas = Volume.milliliters 100.0
    , flutterings = []
    , next = []
    }


setIdleMovements : List QuadraticSpline -> Model -> Model
setIdleMovements generated =
    mapMoth (\m -> { m | flutterings = List.foldl Q.enqueue m.flutterings generated })


updatePosition : Duration -> Moth -> Moth
updatePosition elapsed moth =
    case moth.state of
        Feeding ->
            moth

        Sitting ->
            moth

        Escaping _ ->
            let
                newP =
                    Point2d.translateIn
                        Direction2d.positiveY
                        (Quantity.for elapsed metersPerSecondOfFarting)
                        moth.position
            in
            { moth
                | position = safestMothPosition newP moth.position
            }

        Idle ->
            case ( moth.next, Q.enqueued moth.flutterings ) of
                ( ( p, d, t ) :: rest, _ ) ->
                    let
                        newT =
                            Quantity.difference t elapsed

                        percentT =
                            Duration.inSeconds elapsed / 1

                        newP =
                            Point2d.interpolateFrom moth.position p percentT

                        newD =
                            Direction2d.angleFrom moth.direction d
                                |> flip (Quantity.interpolateFrom (Direction2d.toAngle moth.direction)) percentT
                                |> Direction2d.fromAngle
                    in
                    { moth
                        | position = safestMothPosition newP moth.position
                        , direction = newD
                        , next =
                            if Quantity.lessThanOrEqualToZero newT then
                                rest

                            else
                                ( p, d, newT ) :: rest
                    }

                ( [], True ) ->
                    let
                        ndg =
                            Q.dequeue moth.flutterings
                                |> Maybe.map QuadraticSpline2d.nondegenerate
                                |> Maybe.withDefault (Err Point2d.origin)

                        next =
                            ndg
                                |> Result.map
                                    (\n ->
                                        n
                                            |> QuadraticSpline2d.arcLengthParameterized { maxError = Length.centimeters 1 }
                                            |> QuadraticSpline2d.arcLength
                                            |> Quantity.at_ metersPerSecondIdling
                                            |> Duration.inSeconds
                                            |> ceiling
                                            |> flip Parameter1d.trailing (QuadraticSpline2d.sample n)
                                            |> List.map (\( point, dir ) -> ( Point2d.placeIn (Frame2d.atPoint moth.position) point, dir, Duration.seconds 0.5 ))
                                    )
                                |> Result.withDefault []
                    in
                    { moth
                        | next = next
                        , flutterings = Q.dequeued moth.flutterings
                    }

                _ ->
                    moth

        Farting ->
            let
                newP =
                    Point2d.translateIn
                        moth.direction
                        (Quantity.for elapsed metersPerSecondOfFarting)
                        moth.position
            in
            { moth
                | position = safestMothPosition newP moth.position
            }


safestMothDirection : Direction -> Direction -> Direction
safestMothDirection newDirection fallbackDirection =
    let
        angle =
            Direction2d.toAngle newDirection
    in
    (if between (Angle.degrees 70) (Angle.degrees 110) angle then
        nearest (Angle.degrees 70) (Angle.degrees 110) angle

     else if between (Angle.degrees -110) (Angle.degrees -70) angle then
        nearest (Angle.degrees -110) (Angle.degrees -70) angle

     else
        angle
    )
        |> Direction2d.fromAngle


updateFlowerIndex : Model -> Model
updateFlowerIndex model =
    let
        betweenList l =
            case l of
                first :: second :: _ ->
                    between first second (Point2d.xCoordinate model.moth.position)

                _ ->
                    False
    in
    if anyPredicates [ mothFeedingか, mothSittingか ] model then
        model

    else
        mapFood
            (\f ->
                { f
                    | nextFlowerIndex =
                        (( Normal, Point2d.origin ) :: model.food.flowers)
                            |> List.map (Tuple.second >> Point2d.xCoordinate)
                            |> List.Extra.groupsOfWithStep 2 1
                            |> List.Extra.findIndex betweenList
                }
            )
            model


landOnNearbyFlower : Model -> Model
landOnNearbyFlower model =
    let
        flowerTestState ( index, ( flowerKind, position ) ) =
            { index = index
            , kind = flowerKind
            , hitBox = flowerHitBox ( flowerKind, position )
            }

        mothIntersection =
            BoundingBox2d.intersection (mothHitBox model.moth)

        intersectionArea =
            BoundingBox2d.dimensions >> uncurry Quantity.product
    in
    if anyPredicates [ mothEscapingか, mothFeedingか, mothSittingか ] model then
        model

    else
        model.food.nextFlowerIndex
            |> Maybe.andThen
                ((+) -1
                    >> flip List.drop (List.indexedMap Tuple.pair model.food.flowers)
                    >> List.take 2
                    >> List.map (flowerTestState >> duple)
                    >> List.filterMap (Tuple.mapBoth Just (.hitBox >> mothIntersection >> Maybe.map intersectionArea) >> liftTuple)
                    >> List.Extra.maximumWith
                        (curry
                            (mapEach Tuple.second
                                >> uncurry Quantity.compare
                            )
                        )
                )
            |> Maybe.map
                (\( { kind, hitBox }, iA ) ->
                    if Quantity.greaterThan (Area.squareCentimeters 8) iA then
                        mapMoth
                            (\moth ->
                                { moth
                                    | state = Feeding
                                    , position = Point2d.xy (BoundingBox2d.midX hitBox) (flowerLandingYComponent kind)
                                }
                            )
                            model

                    else
                        model
                )
            |> Maybe.withDefault model


safestMothPosition : Position -> Position -> Position
safestMothPosition newPosition fallBackPosition =
    if mothSubmergingか newPosition then
        Point2d.xy (Point2d.xCoordinate newPosition) (Point2d.yCoordinate fallBackPosition)

    else
        newPosition


mothSubmergingか : Position -> Bool
mothSubmergingか =
    phoenix
        (||)
        (phoenix
            BoundingBox2d.contains
            (Point2d.signedDistanceAlong Axis2d.x >> cardinal Point2d.xy Constants.groundLevel)
            (BoundingBox2d.withDimensions ( mothWidth, mothHeight ))
        )
        (Point2d.signedDistanceAlong Axis2d.y >> Quantity.lessThanZero)


updateState : { isPressed : Bool, controlStateJustChanged : Bool, elapsedTime : Duration } -> Moth -> Moth
updateState { isPressed, controlStateJustChanged, elapsedTime } oldMoth =
    --elapsed controlState oldMoth =
    let
        newlyPressed =
            isPressed && controlStateJustChanged

        mapStateIfGassy fallback fn m =
            m |> defaultTo (mapState (kestrel fallback)) (not <| emptyか m.gas) (mapState fn)

        sitIfFull m =
            m |> defaultTo (kestrel m) (fullか m.gas) (mapGas (kestrel (Volume.milliliters 100.0)) >> mapState (kestrel Sitting))

        emptyか =
            Quantity.lessThanOrEqualToZero

        fullか =
            Quantity.greaterThanOrEqualTo (Volume.milliliters 100.0)

        reduceGas =
            Quantity.minus (Quantity.at Constants.gasPerSecondOfFarting elapsedTime)

        replenishGas =
            Quantity.plus (Quantity.at Constants.gasPerSecondOfFeeding elapsedTime)

        mapGas fn m =
            { m | gas = fn m.gas }

        mapState fn m =
            { m | state = fn m.state }

        reduceGasIfFarting m =
            m |> defaultTo (kestrel m) (m.state == Farting) (mapGas reduceGas)

        replenishGasIfFeeding m =
            m |> defaultTo (kestrel m) (m.state == Feeding) (mapGas replenishGas)

        clearNextIfFarting m =
            { m | next = defaultTo m.next (m.state == Farting) [] }

        idolize m =
            { m | state = Idle }

        transitionFromIdleToFarting =
            defaultTo identity isPressed (mapStateIfGassy Idle (always Farting))
    in
    case oldMoth.state of
        Idle ->
            updatePosition elapsedTime oldMoth
                |> transitionFromIdleToFarting

        Feeding ->
            if newlyPressed then
                mapStateIfGassy Feeding (always startEscaping) oldMoth

            else
                replenishGasIfFeeding oldMoth
                    |> sitIfFull

        Sitting ->
            if newlyPressed then
                mapStateIfGassy Feeding (always startEscaping) oldMoth

            else
                oldMoth

        Escaping remaining ->
            if Quantity.lessThanOrEqualToZero remaining then
                idolize oldMoth

            else
                { oldMoth
                    | state =
                        Escaping
                            (remaining
                                |> Quantity.minus (Quantity.at C.metersPerSecondOfFarting elapsedTime)
                            )
                }
                    |> updatePosition elapsedTime

        Farting ->
            let
                moth =
                    updatePosition elapsedTime oldMoth
            in
            if isPressed then
                reduceGasIfFarting moth
                    |> mapStateIfGassy Idle (always Farting)
                    |> clearNextIfFarting

            else
                idolize moth


{-| Food
-}
type alias Food =
    { flowers : List ( Flower, Position )
    , nextFlowerIndex : Maybe Int

    {- Use this index to store calculated data for:
       1. Making flower in direction of moth glow
       2. Checking quickly the intersection of both flowers with the moth
    -}
    }


type Flower
    = Normal


initFood : Food
initFood =
    { flowers = [ ( Normal, Point2d.centimeters 500 0 ) ]
    , nextFlowerIndex = Just 0
    }


flowerHitBox : ( Flower, Position ) -> Constants.HitBox
flowerHitBox ( flower, position ) =
    case flower of
        Normal ->
            BoundingBox2d.withDimensions
                ( Length.centimeters 50, Quantity.plus (Length.centimeters 50) (flowerHeight flower) )
                (position |> Point2d.translateIn Direction2d.positiveY (flowerHeight flower))


flowerHeight : Flower -> Quantity Float Length.Meters
flowerHeight flower =
    case flower of
        Normal ->
            Length.centimeters 285


flowerLandingYComponent : Flower -> Distance
flowerLandingYComponent flower =
    case flower of
        Normal ->
            Length.centimeters 231


{-| Antagonists
-}
type alias Antagonist =
    { position : Position
    , direction : Direction
    , pursuing : Maybe WalkingPath
    , pursuit : Pursuit
    }


type Pursuit
    = Patrol
    | Alignment Position
    | Pursuit Position
    | Deescalation Position


initBat : Antagonist
initBat =
    { position = Point2d.along C.batFlightAxis (Length.meters -9)
    , direction = Direction2d.positiveX
    , pursuing = Nothing
    , pursuit = Patrol
    }


batHitBox : Antagonist -> HitBox
batHitBox =
    .position >> BoundingBox2d.withDimensions ( C.batWidth, C.batHeight )


batPatrollingか : Antagonist -> Bool
batPatrollingか =
    .pursuing >> Maybe.Extra.isNothing


batSwoopingか : Antagonist -> Bool
batSwoopingか =
    .pursuing >> Maybe.Extra.isJust


{-| bat stuff
when patrolling, if moth enters idle mode above flower height, moth is elligible for attack
if bat is facing moth and at least 45degrees away, then bat will attack
bat swoops when at 45degrees, so if not yet at 45, it enters alignment mode with position = 45degree mark
at 45degree mark, bat angles down towards moth's active position in pursuit mode with position = moth.position
if moth and bat have x amount of hitbox overlap at pursuit position, then it's game over
else, bat enters deescalation with position equals spot on flight axis 45 degrees away
-}
updateBat : Model -> Model
updateBat model =
    let
        check =
            Ok

        chain =
            Result.andThen

        test : (Moth -> Antagonist -> Model -> Result Model Model) -> Result Model Model -> Result Model Model
        test fn =
            chain (\m -> fn m.moth m.bat m)

        succeed =
            Ok

        fail =
            Err

        checkVulnerability =
            test
                (\_ _ m ->
                    if mothIdlingか m then
                        succeed m

                    else
                        fail m
                )

        enterPursuitIfConditionsMet =
            let
                checkEcholocationRange =
                    test
                        (\moth bat m ->
                            if
                                Point2d.distanceFrom moth.position bat.position
                                    |> Quantity.lessThanOrEqualTo C.echolocationRange
                            then
                                succeed m

                            else
                                fail m
                        )

                checkMothInBatDirection =
                    test
                        (\moth bat m ->
                            if
                                Point2d.signedDistanceAlong (Axis2d.through bat.position bat.direction) moth.position
                                    |> Quantity.greaterThanZero
                            then
                                succeed m

                            else
                                fail m
                        )

                checkMothAtCorrectAngle =
                    test
                        (\moth bat m ->
                            let
                                threshold =
                                    Quantity.equalWithin (Angle.degrees 5)
                            in
                            Direction2d.from bat.position moth.position
                                |> Maybe.map
                                    (\d ->
                                        if
                                            Direction2d.angleFrom bat.direction d
                                                |> duple
                                                |> Tuple.mapBoth (threshold (Angle.degrees 45)) (threshold (Angle.degrees 135))
                                                |> uncurry (||)
                                        then
                                            mapBat (\b -> { b | pursuit = Pursuit moth.position }) m
                                                |> succeed

                                        else
                                            fail m
                                    )
                                |> Maybe.withDefault (fail m)
                        )
            in
            duple
                >> Tuple.mapSecond
                    (check
                        >> checkVulnerability
                        >> checkMothInBatDirection
                        >> checkEcholocationRange
                        >> checkMothAtCorrectAngle
                    )
                >> uncurry Result.withDefault

        cancelPursuitIfConditionsMet =
            duple
                >> Tuple.mapBoth
                    (mapBat (\b -> { b | pursuit = Deescalation (Point2d.translateIn Direction2d.positiveY (Point2d.signedDistanceFrom C.batFlightAxis b.position) b.position) }))
                    (check
                        >> checkVulnerability
                    )
                >> uncurry Result.withDefault

        δ =
            tickδ model.system

        turnAroundAtEdges m =
            let
                ( minPt, maxPt ) =
                    ( C.minimalBatRange, C.maximalBatRange ) |> mapEach (Point2d.along C.batFlightAxis)

                batPos =
                    m.bat.position
            in
            case ( Point2d.lexicographicComparison batPos minPt, Point2d.lexicographicComparison batPos maxPt ) of
                ( LT, _ ) ->
                    mapBat (\b -> { b | direction = Direction2d.reverse b.direction }) m

                ( _, GT ) ->
                    mapBat (\b -> { b | direction = Direction2d.reverse b.direction }) m

                _ ->
                    m
    in
    case model.bat.pursuit of
        Patrol ->
            mapBat
                (\b ->
                    { b
                        | position = Point2d.translateIn b.direction (Quantity.at C.batFlightSpeed δ) b.position
                    }
                )
                model
                |> turnAroundAtEdges
                |> enterPursuitIfConditionsMet

        Alignment destination ->
            if Point2d.equalWithin (Length.centimeters 1) model.bat.position destination then
                model
                -- enter pusuit

            else
                model

        -- continue alignment
        Pursuit destination ->
            model
                |> mapBat
                    (\b ->
                        Direction2d.from b.position model.moth.position
                            |> Maybe.map
                                (\d ->
                                    { b
                                        | position = Point2d.translateIn d (Quantity.at C.batSwoopSpeed δ) b.position
                                        , direction = d
                                        , pursuit = Pursuit model.moth.position
                                    }
                                )
                            |> Maybe.withDefault b
                    )
                |> cancelPursuitIfConditionsMet

        Deescalation destination ->
            if Point2d.equalWithin (Length.centimeters 1) model.bat.position destination then
                model
                -- return to patrol mode

            else
                model



-- continue moving


initSpider : Antagonist
initSpider =
    { position = Point2d.centimeters -900 0
    , direction = Direction2d.positiveX
    , pursuing = Nothing
    , pursuit = Patrol
    }


spiderHitBox : Antagonist -> HitBox
spiderHitBox =
    .position >> BoundingBox2d.withDimensions ( C.spiderWidth, C.spiderHeight )


spiderOnGroundか : Antagonist -> Bool
spiderOnGroundか =
    duple
        >> Tuple.mapBoth
            (.position >> Point2d.xCoordinate >> flip Point2d.xy (Length.centimeters 0.1))
            spiderHitBox
        >> uncurry BoundingBox2d.contains


spiderPathToMoth : Moth -> Antagonist -> PursuitPath
spiderPathToMoth moth spider =
    let
        ( spiderXCoord, spiderYCoord ) =
            Point2d.coordinates spider.position

        ( mothXCoord, mothYCoord ) =
            Point2d.coordinates moth.position

        threshold =
            Length.centimeters 0.1

        yZero =
            Length.centimeters 0
    in
    [ spider.position ]
        |> appendIf (not <| spiderOnGroundか spider) (Point2d.xy spiderXCoord yZero)
        |> appendIf (between yZero C.spiderHeight mothYCoord) (Point2d.xy mothXCoord yZero)
        |> flip (++) [ moth.position ]
        |> Polyline2d.fromVertices


updateSpider : Model -> Model
updateSpider model =
    let
        tδ =
            tickδ model.system

        updatePursuit path s =
            { s | pursuing = Just (path s) }

        updateSpiderPosition s =
            let
                ( position, direction ) =
                    case s.pursuing of
                        Just path ->
                            Polyline2d.segments path
                                |> List.head
                                |> Maybe.andThen LineSegment2d.direction
                                |> Maybe.map
                                    (duple
                                        >> Tuple.mapFirst
                                            (finchStar Point2d.translateIn s.position (Quantity.at C.spiderPursuitSpeed tδ))
                                    )
                                |> Maybe.withDefault ( s.position, s.direction )

                        Nothing ->
                            if Point2d.signedDistanceFrom Axis2d.x s.position |> Quantity.greaterThanZero then
                                ( s.position |> Point2d.translateIn Direction2d.negativeY (Quantity.at C.spiderWalkSpeed tδ)
                                , Direction2d.negativeY
                                )

                            else
                                ( s.position, s.direction )
            in
            { s | position = position, direction = direction }

        {-
           meanderSpider fn =
               mapSpider (\s ->
                   let (pos, dir) = fn s.position s.dir
                   in
                   { s | position = pos, direction = dir }
               )
        -}
        chaseMoth m =
            m
                |> mapSpider (updatePursuit (spiderPathToMoth m.moth))
                |> phoenix mapSpider (kestrel << updateSpiderPosition << .spider) identity
    in
    case model.moth.state of
        Sitting ->
            chaseMoth model

        Feeding ->
            chaseMoth model

        _ ->
            if model.moth.position |> Point2d.signedDistanceFrom Axis2d.x |> Quantity.greaterThan C.spiderHeight then
                model
                {- if Point2d.lexicographicComparison model.moth.position model.spider.position == GT then
                   meanderSpider (Tuple.pair >> Tuple.mapBoth
                -}

            else
                chaseMoth model
