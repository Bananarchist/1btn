module Model exposing
    ( Model, Flower(..), Grass(..)
    , init, setGrassAnimations, grassAnimationsCompleteか, anyPendingMothMovementsか, setMothMovements, currentTickか, addNotification
    , anyNotifications, notification, seaCoords
    , gas, mothRot, mothScale, mothPos, farts, gameOverか, mothMovingか, mothLandingか, mothFartingか, mothFeedingか, mothTakingOffか
    , birdScale, birdPos, lizardPos, lizardScale, loverPos, loverScale
    , visibleGrassSegments, fireflies, trees, flowers
    , anyPressedか, listeningか, padCount
    , updateMothStatus, updateGameState
    , lizardPosΔ, birdPosΔ, loverPosΔ, setLover
    , setGrass, setFlowers
    , setWindow
    , setKeyboard, setMouse, setPad, setListeningPad
    , birdCoords, framePos, lizardCoords, loverCoords, mothCoords, mothMirroredか, mothθ, setCurrentTick, setLastTick, setListeningControls
    )

{-| Model and API


## Types

@docs Model, MothState, Flower, Grass


## Constructors

@docs init


## Selectors


### System

@docs anyNotifications, notification, currentTickか


### Moth

@docs mothState, gas, mothRot, mothScale, mothPos, farts, gameOverか, mothMovingか, mothLandingか, mothFartingか, mothFeedingか, mothTakingOffか


### NPCs

@docs birdScale, birdPos, lizardPos, lizardScale, loverPos, loverScale


### Map

@docs visibleGrassSegments, fireflies, trees, flowers


### Controls

@docs anyPressedか, listeningか, padCount


## Mutators


### System

@docs setTick


### Moth

@docs updateMothPos, updateMothStatus, updateGameState


### NPCs

@docs lizardPosΔ, birdPosΔ, loverPosΔ, setLover


### Map

@docs setGrass, setFlowers


### Graphics

@docs setWindow


### Controls

@docs setKeyboard, setMouse, setPad, setListeningPad, setListeningPad

-}

import Axis2d
import Direction2d
import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Duration exposing (Duration)
import Frame2d exposing (Frame2d)
import Helpers exposing (allPredicates, anyPredicates, duple, flip, uncurry, within)
import Length exposing (Length, Meters)
import Pixels
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Queue as Q
import Speed exposing (Speed)
import Vector2d exposing (Vector2d)


{-| Constants
-}
gasMeters : Float
gasMeters = 0.5

gasSpeed : Length.Length
gasSpeed =
    Length.meters gasMeters


cameraLag : Duration
cameraLag =
    Duration.milliseconds 400


fartVelocity : Speed.Speed
fartVelocity =
    Speed.metersPerSecond gasMeters

groundLevel : Float
groundLevel = -700

halfPi : Float
halfPi = pi / 2

quarterPi : Float
quarterPi = pi / 4

tau : Float
tau = 2 * pi


gasDepletion : Float
gasDepletion =
    10


{- Types -}
type Grass
    = NormalGrass
    | DenseGrass
    | TallGrass
    | FloweringGrass
    | CloveredGrass
    | ShoreLineGrass




type alias Model =
    { controls : Controls
    , positions : Positions
    , movements : Movements
    , gas : Float
    , direction : Vector
    , θ : Angle
    , lastTick : Duration
    , currentTick : Duration
    , width : Int
    , height : Int
    , random : RandomValues
    , notifications : List (Duration, String)
    }


type alias Controls =
    { kb : Bool
    , mouse : Bool
    , gp : Bool
    , input : Bool
    , padCount : Int
    }


type World
    = World

type Screen
    = Screen

type Local
    = Local


type alias Position =
    Point2d Meters World

type alias ScreenPosition =
    Point2d Meters Screen

type alias Vector =
    Vector2d Meters World


type alias Frame =
    Frame2d Meters World { defines : Screen }


type alias Coordinates =
    ( Quantity Float Meters, Quantity Float Meters )


type alias Fart =
    { initial : Position
    , θ : Angle
    , length : Int
    , created : Duration
    , active : Bool
    }

type alias Positions =
    { moth : Position
    , flowers : List ( Flower, Position )
    , grass : List ( Bool, Grass )
    , lover : Position
    , frame : Frame
    , farts : List Fart
    }

type alias RandomValues =
    { grass : (Duration, List Bool) }

type Flower
    = Flower


type alias Movement =
    { time : ( Duration, Duration )
    , coords : ( Position, Position )
    , angles : ( Angle, Angle )
    , direction : Direction2d.Direction2d World
    }


{-| type MothAnimation
= Flutter Remaining
| FlutterTurn Remaining
| StartFart Remaining
| LandOn ( Float, Float ) Remaining
-}
type alias Movements =
    { moth : Q.Queue Movement
    , lover : Q.Queue Movement
    , frame : Q.Queue Movement
    }



{-
   Now moth is fluttering if fart test fails
   And when a flutter movement is depleted it
   is replaced with a new one
   And when farting is enabled
   it is replaced with the wind up and the fart after that
-}


init : Int -> Int -> Float -> Model
init width height currentTick_ =
    { controls = initControls
    , positions = initPositions width height
    , movements = initMovements
    , gas = 100.0
    , θ = Angle.radians quarterPi
    , direction = Vector2d.rTheta gasSpeed (Angle.radians (negate quarterPi))
    , lastTick = Duration.seconds currentTick_
    , currentTick = Duration.seconds currentTick_
    , width = width
    , height = height
    , random = initRandomValues
    , notifications = []
    }


initControls : Controls
initControls =
    { kb = False, mouse = False, gp = False, input = True, padCount = 0 }


initPositions : Int -> Int -> Positions
initPositions width height =
    let
        screenWMid = toFloat width / 2
        screenHMid = toFloat height / 2

        initialFrame =
            --Frame2d.atPoint (Point2d.centimeters (screenWMid - 32) (screenHMid - 32)) |> Frame2d.reverseY
            Frame2d.atPoint (Point2d.centimeters 0 0) |> Frame2d.reverseY

        initialMothPos =
            --Point2d.xyIn initialFrame (Length.centimeters 0) (Length.centimeters 0)
            Point2d.centimeters (screenWMid - 32) (screenHMid - 32 |> negate)
        initialLoverPos =
            Point2d.xyIn initialFrame (Length.centimeters 1000) (Length.centimeters 1000)

        -- Point2d.centimeters 1000 1000 --Point2d.pixels 1000 1000
    in
    { moth = initialMothPos
    , flowers = []
    , grass = []
    , lover = initialLoverPos
    , frame = initialFrame
    , farts = []
    }


initMovements : Movements
initMovements =
    { moth = []
    , lover = []
    , frame = []
    }
    
initRandomValues : RandomValues
initRandomValues =
    { grass = (Duration.seconds 0, List.repeat 100 False) }



{- Selectors -}


{-| System
-}
anyNotifications : Model -> Bool
anyNotifications =
    .notifications >> List.isEmpty >> not


notification : Model -> Maybe String
notification =
    .notifications >> List.head >> Maybe.map Tuple.second


lastTick : Model -> Duration
lastTick =
    .lastTick


currentTick : Model -> Duration
currentTick =
    .currentTick


tickδ : Model -> Duration
tickδ =
    duple
        >> Tuple.mapBoth currentTick lastTick
        >> uncurry Quantity.difference

currentTickか : Model -> Bool
currentTickか =
    currentTick >> Quantity.greaterThanZero



mothMovingか : Model -> Bool
mothMovingか model =
    False


mothFartingか : Model -> Bool
mothFartingか model =
    gas model > 0 && anyPressedか model

fartActiveか : Model -> Bool
fartActiveか model =
    case model.positions.farts of
        f :: fs ->
            f.active
        _ ->
            False


mothLandingか : Model -> Bool
mothLandingか model =
    False


mothTakingOffか : Model -> Bool
mothTakingOffか model =
    False


anyPendingMothMovementsか : Model -> Bool
anyPendingMothMovementsか =
    .movements
    >> .moth
    >> List.isEmpty
    >> not

framePos : Model -> Position
framePos model =
    let
        ( spanW, spanH ) =
            ( model.width // 2 |> toFloat, model.height // 2 |> toFloat )

        lowerPoint =
            mothPos model |> Point2d.translateBy (Vector2d.centimeters -spanW spanH)

        upperPoint =
            mothPos model |> Point2d.translateBy (Vector2d.centimeters spanW -spanH)
    in
    model.positions.frame |> Frame2d.originPoint

--mothState model |> Tuple.second |> (==) Feeding


mothFeedingか : Model -> Bool
mothFeedingか model =
    False



-- mothState model |> (==) Feeding


usingGasか : Model -> Bool
usingGasか model =
    anyPressedか model && gas model > 0


gas : Model -> Float
gas =
    .gas



mothPos : Model -> Position
mothPos =
    .positions >> .moth


mothCoords : Model -> Coordinates
mothCoords model =
    mothPos model
    --|> Point2d.coordinates
    |> Point2d.coordinatesIn model.positions.frame




mothθ : Model -> Angle
mothθ =
    .θ


mothRot : Model -> Angle
mothRot model =
    if model.direction |> Vector2d.yComponent |> Quantity.lessThanZero then
        Angle.radians halfPi
    else 
        Angle.radians 0
               

mothScale : Model -> Float
mothScale =
    always 1.0


mothMirroredか : Model -> Bool
mothMirroredか =
    .direction 
        >> Vector2d.xComponent
        >> Quantity.lessThanZero


mothMovements : Model -> Q.Queue Movement
mothMovements =
    .movements >> .moth


farts : Model -> List (List { coords : Coordinates, scale : Float, rotation : Angle })
farts model =
    model.positions
    |> .farts
    |> List.map
        (\{initial, θ, length} ->
            let
                --(dir, len) = ((Direction2d.fromAngle θ |> Direction2d.relativeTo model.positions.frame), (Length.centimeters 1))
                (dir, len) = ((Direction2d.fromAngle θ), (Length.centimeters 1)) |> Debug.log "fart direction"
                initialFart = initial |> Point2d.relativeTo model.positions.frame --Point2d.coordinatesIn model.positions.frame
            in
            { coords = initialFart |> Point2d.coordinates, scale = 1, rotation = θ }
            :: (List.range 1 length |> List.map (\i -> 
                { coords = 
                    initialFart
                    --|> Point2d.relativeTo model.positions.frame
                    |> Point2d.translateIn (dir |> Direction2d.relativeTo model.positions.frame) (Quantity.multiplyBy (toFloat i) len) 
                    |> Point2d.coordinates
                    --|> Point2d.coordinatesIn model.positions.frame
                , scale = 1
                , rotation = θ
                }
                ))
            )
    -- always [[ { pos = Point2d.centimeters 0 0, scale = 1 } ]]


gameOverか : Model -> Bool
gameOverか =
    always False


proximalToLoverか : Model -> Bool
proximalToLoverか =
    always True


proximalToLizardか : Model -> Bool
proximalToLizardか =
    always True


proximalToBirdか : Model -> Bool
proximalToBirdか =
    always True


proximalToIcarusか : Model -> Bool
proximalToIcarusか =
    always True


{-| NPCs
-}
lizardPos : Model -> Position
lizardPos =
    always (Point2d.centimeters 0 0)


lizardCoords : Model -> Coordinates
lizardCoords =
    duple
        >> Tuple.mapSecond lizardPos
        >> uncurry coordinatesIn


lizardScale : Model -> Float
lizardScale =
    always 1.0


birdPos : Model -> Position
birdPos =
    always (Point2d.centimeters 0 0)


birdCoords : Model -> Coordinates
birdCoords =
    duple
        >> Tuple.mapSecond birdPos
        >> uncurry coordinatesIn


birdScale : Model -> Float
birdScale =
    always 1.0


loverPos : Model -> Position
loverPos =
    .positions
        >> .lover


loverCoords : Model -> Coordinates
loverCoords =
    duple
        >> Tuple.mapSecond loverPos
        >> uncurry coordinatesIn


loverScale : Model -> Float
loverScale =
    always 1.0


{-| Map
-}
coordinatesIn : Model -> Position -> Coordinates
coordinatesIn model =
    Point2d.coordinatesIn model.positions.frame


visibleGrassSegments : Model -> List { kind : Grass, coords : Coordinates, scale : Float, animated : Bool, mirrored : Bool}
visibleGrassSegments model =
    model
        |> duple
        |> Tuple.mapBoth (.positions >> .grass) (.random >> .grass >> Tuple.second)
        |> uncurry (List.map3 (\idx ( f, k ) animate -> 
            { kind = k
            --, coords = (Length.centimeters (toFloat idx |> (*) 32), Length.centimeters groundLevel)
            , coords = Point2d.xyIn model.positions.frame (toFloat idx |> (*) 32 |> Length.centimeters) (Length.centimeters groundLevel) |> Point2d.coordinates
            , scale = 1 
            , animated = animate
            , mirrored = f
            }) 
            (List.range 0 (List.length model.positions.grass)))


fireflies : Model -> List { pos : Position, scale : Float }
fireflies =
    always [ { pos = Point2d.centimeters 0 0, scale = 1 } ]


trees : Model -> List { pos : Position, scale : Float }
trees =
    always [ { pos = Point2d.centimeters 0 0, scale = 1 } ]


flowers : Model -> List { kind : Flower, coords : Coordinates, scale : Float }
flowers model =
    [] --[ { kind = Flower, coords = coordinatesIn model (Point2d.centimeters 0 0), scale = 1 } ]


flowerYPos : Flower -> Float
flowerYPos =
    always 20

seaPos : Position
seaPos =
    Point2d.centimeters 0 groundLevel 

seaCoords : Model -> Coordinates
seaCoords model =
    Point2d.xyIn model.positions.frame (Length.centimeters -1000) (Length.centimeters (groundLevel - 20)) 
    |> Point2d.coordinates

grassAnimationsCompleteか : Model -> Bool
grassAnimationsCompleteか = 
    duple
    >> Tuple.mapFirst (.random >> .grass >> Tuple.first)
    >> Tuple.mapSecond (currentTick)
    >> uncurry (Quantity.equalWithin (Duration.seconds 2))
    >> not

{-| Input
-}
anyPressedか : Model -> Bool
anyPressedか model =
    if listeningか model then
        controls model
            |> anyPredicates [ .kb, .mouse, .gp ]

    else
        False


controls : Model -> Controls
controls =
    .controls


listeningか : Model -> Bool
listeningか =
    .controls >> .input


padCount : Model -> Int
padCount =
    .controls
        >> .padCount


{-| Mutators
-}
mapModel : (Model -> Model) -> Model -> Model
mapModel fn m =
    fn m


mapPositions : (Positions -> Positions) -> Model -> Model
mapPositions fn =
    mapModel (\m -> { m | positions = fn m.positions })


mapGas : (Float -> Float) -> Model -> Model
mapGas fn =
    mapModel (\m -> { m | gas = fn m.gas })


mapMovements : (Movements -> Movements) -> Model -> Model
mapMovements fn =
    mapModel (\m -> { m | movements = fn m.movements })


{-| System
-}
setLastTick : Duration -> Model -> Model
setLastTick t =
    mapModel (\m -> { m | lastTick = t })


setCurrentTick : Duration -> Model -> Model
setCurrentTick t =
    mapModel (\m -> { m | currentTick = t })

addNotification : String -> Model -> Model
addNotification msg =
    mapModel (\m -> { m | notifications = (Duration.seconds 4, msg) :: m.notifications })

updateNotifications : Model -> Model
updateNotifications model =
    case model.notifications of
        (nd, nmsg) :: ns ->
            if Quantity.greaterThanZero nd then
                mapModel (\m -> { m | notifications = (Quantity.minus (tickδ model) nd, nmsg) :: ns}) model
            else
                mapModel (\m -> {m | notifications = ns}) model
        _ ->
            model

{-| MOTH
-}
mapMothPosition : (Position -> Position) -> Model -> Model
mapMothPosition fn =
    mapPositions (\p -> { p | moth = fn p.moth })


mapFramePosition : (Frame -> Frame) -> Model -> Model
mapFramePosition fn =
    mapModel (\m ->
        let newFrame = fn m.positions.frame 
            oldFrame = m.positions.frame
            newPositions p f = { p | frame = f }
            newModel f = { m | positions = newPositions m.positions f }
        in
        if newFrame 
            |> Frame2d.originPoint 
            |> Point2d.coordinatesIn oldFrame
            |> Tuple.second 
            --|> Debug.log "frame y coord"
            |> Quantity.plus (Length.centimeters (toFloat m.height + 32)) -- |> Debug.log "ground level y coord")
            |> Quantity.greaterThan (Length.centimeters groundLevel) then
                if oldFrame
                    |> Frame2d.originPoint 
                    |> Point2d.coordinatesIn oldFrame
                    |> Tuple.second 
                    --|> Debug.log "frame y coord"
                    |> Quantity.plus (Length.centimeters (toFloat m.height + 32)) -- |> Debug.log "ground level y coord")
                    |> Quantity.greaterThan (Length.centimeters groundLevel) then
                        newModel newFrame --newModel oldFrame -- figure out how to adjust
                else
                    newModel newFrame -- m 
        else
            newModel newFrame 
        )



mapMothRotation : (Angle -> Angle) -> Model -> Model
mapMothRotation fn =
    mapModel (\m -> { m | θ = m |> mothθ |> fn })




updateMothMovement : Float -> Model -> Model
updateMothMovement tick model =
    model

setMothMovements : List { xComponent : Length, yComponent : Length, θ : Angle } -> Model -> Model
setMothMovements values model =
    if mothFartingか model then
        model
    else
        mapMothMovements
            (flip (List.foldl (flip Q.enqueue))
                (List.indexedMap 
                    (\idx -> 
                        ( newMovement idx model 
                        >> Tuple.mapBoth List.singleton List.singleton 
                        >> uncurry (++)) 
                    )
                    values
                |> List.concatMap identity
                )
            )
            model

newMovement : Int -> Model -> { xComponent : Length, yComponent : Length, θ : Angle } -> (Movement, Movement)
newMovement offset model {xComponent, yComponent, θ} =
    let
        timeOffset = 3000 + (offset * 3000) |> toFloat
        currentTick_ = currentTick model |> Quantity.plus (Duration.milliseconds (offset * 3000 |> toFloat))
        newTime = Quantity.plus (Duration.milliseconds timeOffset) currentTick_
        currentPos_ = mothPos model
        vector = Vector2d.xy xComponent yComponent
        newPt = Point2d.translateBy vector currentPos_
        currentθ = mothθ model
        newθ = Quantity.plus θ currentθ
        newDir = 
            case (Quantity.lessThanZero xComponent, Quantity.lessThanZero yComponent) of
                (True, True) -> Direction2d.fromAngle (Angle.degrees 225)
                (True, False) -> Direction2d.fromAngle (Angle.degrees 225)
                (False, True) -> Direction2d.fromAngle (Angle.degrees 45)
                (False, False) -> Direction2d.fromAngle (Angle.degrees -45)
                {-
                (True, True) -> Direction2d.fromAngle (Angle.degrees -45)
                (True, False) -> Direction2d.fromAngle (Angle.degrees -135)
                (False, True) -> Direction2d.fromAngle (Angle.degrees 45)
                (False, False) -> Direction2d.fromAngle (Angle.degrees 135)
                -}
    in
    (
        { time = (currentTick_, newTime)
        , coords = (currentPos_, newPt)
        , angles = (currentθ, newθ)
        , direction = newDir
        }
    ,   { time = (newTime, Quantity.plus (Duration.milliseconds 3000) newTime)
        , coords = (newPt, currentPos_)
        , angles = (newθ, currentθ)
        , direction = Direction2d.reverse newDir
        }
    )

updateGas : Float -> Model -> Model
updateGas factor =
    duple
        >> Tuple.mapFirst
            (tickδ
                >> Duration.inSeconds
                >> (*) factor
                >> (*) gasDepletion
            )
        >> Tuple.mapFirst (\d -> (+) d >> clamp 0 100)
        >> uncurry mapGas


depleteGas : Model -> Model
depleteGas =
    updateGas -1


replenishGas : Model -> Model
replenishGas =
    updateGas 1


updateMothGas : Model -> Model
updateMothGas model =
    if usingGasか model then
        depleteGas model

    else if mothFeedingか model then
        replenishGas model

    else
        model


addFlutterMovements : Model -> Model
addFlutterMovements =
    identity



-- enqueue some flutter


updateMothPositions : Model -> Model
updateMothPositions model =
    let
        movimientos =
            model |> mothMovements
    in
    if mothFartingか model then
        let
            mothPosRelative = mothPos model |> Point2d.relativeTo model.positions.frame
            vector = model.direction |> Vector2d.scaleBy (tickδ model |> Duration.inSeconds)
            vectorRelative = model.direction |> Vector2d.relativeTo model.positions.frame |> Vector2d.scaleBy (tickδ model |> Duration.inSeconds)
            mappedMoth = mapMothPosition (vector {- |> Vector2d.reverse  |> Vector2d.mirrorAcross (Axis2d.y)-} |> Point2d.translateBy) model
            --mappedMoth = mapMothPosition (always (Point2d.translateBy vectorRelative mothPosRelative |> Point2d.placeIn model.positions.frame)) model
            --mappedFrame = mapFramePosition (Frame2d.translateBy vector) model
            --mappedMoth = mapMothPosition (always (Frame2d.originPoint mappedFrame.positions.frame)) mappedFrame
        in
        mappedMoth
        --model
        --|> mapMothPosition (Point2d.translateBy vector)
        |> mapFramePosition (vector |> Vector2d.mirrorAcross Axis2d.y |> Frame2d.translateBy )
        --mapFramePosition (Frame2d.moveTo (mothPos mappedMoth)) mappedMoth
        |> generateFarts

    else if Q.enqueued movimientos then
        case Q.dequeue movimientos of
            Just { time, coords, angles, direction } ->
                --start, origin, end, destination, firstAngle, rotation } ->
                let
                    mθ = mothθ model
                    mothAngle = 
                        if Quantity.greaterThan (Angle.radians pi) mθ then
                            mθ
                        else
                            Quantity.minus (Angle.radians halfPi) mθ
                    ( start, end ) =
                        time

                    ( source, destination ) =
                        coords

                    ( oriented, orienting ) =
                        angles

                    percentComplete =
                        (currentTick model |> Quantity.minus start |> Duration.inMilliseconds) / (end |> Quantity.minus start |> Duration.inMilliseconds)

                    vector =
                        Vector2d.from source destination |> Vector2d.scaleBy (percentComplete * 0.02) |> Vector2d.rotateBy (Angle.radians (negate halfPi))
                   
                    newDirection = 
                        Vector2d.withLength gasSpeed direction

                in
                model
                --mapFramePosition (Frame2d.translateBy vector) model
                --mapMothPosition (always (Point2d.interpolateFrom source destination percentComplete)) model
                |> mapMothPosition (Point2d.translateBy (Vector2d.mirrorAcross Axis2d.x vector))
                |> setMothDirection newDirection
                |> deactivateFart

            Nothing ->
                model
                |> deactivateFart

    else
        model
        |> deactivateFart

setMothDirection : Vector -> Model -> Model
setMothDirection vector =
    mapModel (\m -> { m | direction = vector })

updateMothRotation : Model -> Model
updateMothRotation model =
    if Q.enqueued model.movements.moth then
        model
        -- use that I guess

    else
        model

newFart : Model -> Fart
newFart model =
    let
        (xc, yc) = model.direction |> Vector2d.components
        θ = model.direction 
            |> Vector2d.relativeTo model.positions.frame 
            |> Vector2d.direction 
            |> Maybe.map Direction2d.toAngle 
            |> Maybe.withDefault (Angle.degrees 45)
        translatedPoint =
            mothPos model
            |> Point2d.translateBy (Vector2d.centimeters -32 -10)
            --Vector2d.rotateBy θ model.direction
            --|> Point2d.translateBy
            {- case (Quantity.greaterThanZero xc, Quantity.greaterThanZero yc) of
                {-(True, True) -> (Point2d.translateBy (Vector2d.centimeters -22 -32), Angle.degrees -45)
                (True, False) -> (Point2d.translateBy (Vector2d.centimeters 32 22), Angle.degrees 45)
                (False, True) -> (Point2d.translateBy (Vector2d.centimeters 22 32), Angle.degrees -135)
                (False, False) -> (Point2d.translateBy (Vector2d.centimeters 59 0 ), Angle.degrees 135)-}
                (True, True) -> (Point2d.translateBy (Vector2d.centimeters -22 -32))
                (True, False) -> (Point2d.translateBy (Vector2d.centimeters 32 22))
                (False, True) -> (Point2d.translateBy (Vector2d.centimeters 22 32))
                (False, False) -> (Point2d.translateBy (Vector2d.centimeters 59 0 ))
                -}
    in
    { initial = Point2d.rotateAround (mothPos model) θ translatedPoint --mothPos model |> translation -- model.positions.frame |> Frame2d.originPoint |> translation
    , θ = θ
    , length = tickδ model |> Quantity.at fartVelocity |> Length.inCentimeters |> floor
    , created = currentTick model
    , active = True 
    } 


generateFarts : Model -> Model
generateFarts model =
    case model.positions.farts of
        fart :: oldFarts ->
            if fart.active then
                let newLength =
                        fart.created 
                        |> flip Quantity.minus (currentTick model) 
                        |> Quantity.at fartVelocity 
                        |> Length.inCentimeters
                in
                mapPositions (\p -> { p | farts = {fart | length = floor newLength } :: oldFarts}) model
            else 
                mapPositions (\p -> { p | farts = newFart model :: fart :: oldFarts }) model
        [] ->
                mapPositions (\p -> { p | farts = newFart model |> List.singleton }) model






mapMothMovements : (Q.Queue Movement -> Q.Queue Movement) -> Model -> Model
mapMothMovements fn =
    mapMovements (\m -> { m | moth = fn m.moth })


cullCompletedMothMovements : Model -> Model
cullCompletedMothMovements model =
    if mothFartingか model then
        mapMothMovements Q.clear model

    else
        case model |> mothMovements |> Q.dequeue of
            Just { time, angles } ->
                if Quantity.greaterThan (Tuple.second time) (currentTick model) then
                    mapMothMovements Q.dequeued model
                        |> mapMothRotation (always (Tuple.second angles))

                else
                    model

            Nothing ->
                model



updateMothGeometry : Model -> Model
updateMothGeometry model =
    updateMothPositions model
        |> updateMothRotation


updateMothAction : Model -> Model
updateMothAction =
    identity


addNewMovements : Model -> Model
addNewMovements =
    identity


updateMothAnimation : Model -> Model
updateMothAnimation =
    cullFarts

deactivateFart : Model -> Model
deactivateFart model =
    case model.positions.farts of
        f1_ :: farts_ ->
            if f1_.active then
                mapPositions (\p ->
                    { p 
                    | farts = { f1_ | active = False } :: farts_
                    }
                ) model
            else
                model
        _ ->
            model

cullFarts : Model -> Model
cullFarts model =
    mapPositions (\p -> 
        { p 
        | farts = List.filter (.created >> Quantity.plus (Duration.seconds 5) >> Quantity.greaterThan (currentTick model)) p.farts
        })
    model

updateMothStatus : Model -> Model
updateMothStatus model =
    updateMothGas model
        |> cullCompletedMothMovements
        |> updateMothGeometry
        |> updateMothAction
        |> addNewMovements
        |> updateMothAnimation
        |> updateNotifications




updateGameState : Model -> Model
updateGameState model =
    if proximalToLoverか model then
        startGoodEnding model

    else if proximalToLizardか model then
        startLizardEnding model

    else if proximalToBirdか model then
        startBirdEnding model

    else if proximalToIcarusか model then
        startIcarusEnding model

    else
        model





startGoodEnding : Model -> Model
startGoodEnding =
    identity


startLizardEnding : Model -> Model
startLizardEnding =
    identity


startBirdEnding : Model -> Model
startBirdEnding =
    identity


startIcarusEnding : Model -> Model
startIcarusEnding =
    identity


{-| NPCs
-}
birdPosΔ : Model -> Model
birdPosΔ =
    identity


lizardPosΔ : Model -> Model
lizardPosΔ =
    identity


loverPosΔ : Model -> Model
loverPosΔ =
    identity


setLover : ( Float, Float ) -> Model -> Model
setLover pos =
    mapPositions (\p -> { p | lover = uncurry Point2d.centimeters pos })


{-| Map
-}
setGrass : List ( Bool, Grass ) -> Model -> Model
setGrass grasses model =
    mapPositions
        (\p ->
            { p
                | grass =
                    ( False, ShoreLineGrass )
                    :: grasses
            }
        )
        model

setGrassAnimations : List Bool -> Model -> Model
setGrassAnimations values model =
    mapRandomValues (\rvs -> { rvs | grass = (model.currentTick, values)}) model

mapRandomValues : (RandomValues -> RandomValues) -> Model -> Model
mapRandomValues fn =
    mapModel (\m -> { m | random = fn m.random })

setFlowers : List ( Flower, Float ) -> Model -> Model
setFlowers fleurs =
    mapPositions (\p -> { p | flowers = List.map (\( f, x ) -> ( f, Point2d.centimeters x (flowerYPos f) )) fleurs })


{-| Output
-}
setWindow : Int -> Int -> Model -> Model
setWindow newWidth newHeight =
    mapModel (\m -> { m | width = newWidth, height = newHeight })
        >> mapFramePosition (always (Frame2d.atPoint (Point2d.centimeters (newWidth |> toFloat) (newHeight |> toFloat))))
        -- should CHECK


{-| Input
-}
mapControls : (Controls -> Controls) -> Model -> Model
mapControls fn =
    mapModel (\m -> { m | controls = fn m.controls })


setKeyboard : Bool -> Model -> Model
setKeyboard bool =
    mapControls (\c -> { c | kb = bool })


setMouse : Bool -> Model -> Model
setMouse bool =
    mapControls (\c -> { c | mouse = bool })


setPad : Bool -> Model -> Model
setPad bool =
    mapControls (\c -> { c | gp = bool })


setListeningPad : Bool -> Model -> Model
setListeningPad bool =
    if bool then
        incrPadCount

    else
        decrPadCount


incrPadCount : Model -> Model
incrPadCount =
    mapControls (\c -> { c | padCount = c.padCount + 1 })


decrPadCount : Model -> Model
decrPadCount =
    mapControls (\c -> { c | padCount = c.padCount - 1 })


setListeningControls : Bool -> Model -> Model
setListeningControls bool =
    mapControls (\c -> { c | input = bool })
