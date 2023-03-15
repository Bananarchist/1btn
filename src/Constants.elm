module Constants exposing (..)


import Arc2d
import Axis2d exposing (Axis2d)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Duration exposing (Duration, Seconds)
import Frame2d exposing (Frame2d)
import Length exposing (Length, Meters)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d
import Quantity exposing (Quantity)
import Queue as Q
import Speed exposing (Speed)
import Vector2d exposing (Vector2d)
import Quantity exposing (Rate)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Volume exposing (Volume, CubicMeters)
import Polyline2d exposing (Polyline2d)

type World
    = World

type Screen
    = Screen

type Local
    = Local

type alias Position =
    Point2d Meters World

type alias Direction = 
    Direction2d World

type alias ScreenPosition =
    Point2d Pixels Screen

type alias Vector =
    Vector2d Meters World

type alias HitBox = 
    BoundingBox2d Meters World

type alias Frame =
    Frame2d Pixels World { defines : Screen }

type alias QuadraticSpline =
    QuadraticSpline2d Meters World

type alias Coordinates =
    ( Quantity Float Meters, Quantity Float Meters )

type alias Gas =
    Volume

type alias Distance =
    Quantity Float Meters

type alias WalkingPath =
    Polyline2d Meters World

type alias FlightPath =
    Polyline2d Meters World

type alias PursuitPath =
    Polyline2d Meters World

type alias Velocity
    = Quantity Float (Rate Meters Seconds)

type alias Axis
    = Axis2d Meters World

metersPerSecondOfFarting : Velocity
metersPerSecondOfFarting = Quantity.rate (Length.centimeters 325) Duration.second

metersPerSecondIdling : Velocity
metersPerSecondIdling = Quantity.rate (Length.centimeters 95) Duration.second

pixelsPerCentimeter : Quantity Float (Rate Meters Pixels)
pixelsPerCentimeter = Quantity.rate (Length.centimeters 1) (Pixels.pixels 1)

gasPerSecondOfFarting : Quantity Float (Rate CubicMeters Seconds)
gasPerSecondOfFarting = Quantity.rate (Volume.milliliters 15.0) Duration.second

gasPerSecondOfFeeding : Quantity Float (Rate CubicMeters Seconds)
gasPerSecondOfFeeding = Quantity.rate (Volume.milliliters 15.0) Duration.second

notificationDuration : Duration
notificationDuration = Duration.seconds 3

groundLevel : Distance
groundLevel = Length.centimeters -5

mothWidth : Distance
mothWidth = Length.centimeters 74

mothHeight : Distance
mothHeight = Length.centimeters 40

spiderWalkSpeed : Velocity
spiderWalkSpeed = Quantity.rate (Length.centimeters 70) Duration.second

spiderPursuitSpeed : Velocity
spiderPursuitSpeed = Quantity.rate (Length.centimeters 140) Duration.second

spiderWidth : Distance
spiderWidth = Length.centimeters 100

spiderHeight : Distance
spiderHeight = Length.centimeters 100

batSwoopSpeed : Velocity
batSwoopSpeed = Quantity.rate (Length.centimeters 325) Duration.second

batFlightSpeed : Velocity
batFlightSpeed = Quantity.rate (Length.centimeters 300) Duration.second

batFlightAxis : Axis 
batFlightAxis = Axis2d.x |> Axis2d.translateIn Direction2d.positiveY (Length.meters 11)

batWidth : Distance
batWidth = Length.centimeters 225

batHeight : Distance
batHeight = Length.centimeters 85

minimalBatRange : Distance
minimalBatRange = Length.meters -40

maximalBatRange : Distance
maximalBatRange = Length.meters 140

echolocationRange : Distance
echolocationRange = Length.meters 20
