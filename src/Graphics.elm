module Graphics exposing (attributes, clipPath, graphicsSubs, mirrorX, mirrorY, rotate, scale, transform, translate, updateGraphicCmds)

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Constants exposing (ScreenPosition) 
import Dict
import Dict.Extra
import Helpers exposing (consIf, flip, uncurry)
import Html exposing (Attribute)
import Html.Attributes as Hats
import Length exposing (Length, Meters)
import Model exposing (Model) --, ScreenPosition)
import Msg exposing (Msg)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Pixels exposing (Pixels)


type alias TransducerState a =
    { initial : a
    , translation : Maybe ( Quantity Float Pixels, Quantity Float Pixels )
    , rotation : Maybe Angle.Angle
    , scale : Maybe ( Float, Float ) 
    }


paintingFrames : Model -> Bool
paintingFrames =
    always True


graphicsSubs : Model -> List (Sub Msg)
graphicsSubs model =
    consIf (paintingFrames model) Msg.animationFrameSub []


updateGraphics : Msg -> Model -> Model
updateGraphics msg model =
    case msg of
        Msg.ResizedWindow newWidth newHeight ->
            Model.setWindow (Pixels.int newWidth) (Pixels.int newHeight) model --Model.setWindow newWidth newHeight model

        _ ->
            model


updateGraphicCmds : Msg -> Model -> List (Cmd Msg)
updateGraphicCmds =
    always <| always [ Cmd.none ]



{-
   scale : Float -> List (Attribute Msg)
   scale factor = []
-}


clipPath : BoundingBox2d Length.Meters coordinates -> List (Attribute Msg)
clipPath =
    always []


transform : a -> TransducerState a
transform a =
    { initial = a, translation = Nothing, rotation = Nothing, scale = Nothing }


translate : (a -> ScreenPosition) -> TransducerState a -> TransducerState a
translate mapper state =
    let
        (x, y) =
            mapper state.initial
            |> Point2d.coordinates
    in
    case state.translation of
        Just ( oldX, oldY ) ->
            { state | translation = Just ( Quantity.plus oldX x, Quantity.plus oldY y ) }

        Nothing ->
            { state | translation = Just ( x, y ) }


rotate : (a -> Angle.Angle) -> TransducerState a -> TransducerState a
rotate mapper state =
    let
        newA =
            mapper state.initial
    in
    case state.rotation of
        Just a ->
            { state | rotation = Just (Quantity.plus newA a) }

        Nothing ->
            { state | rotation = Just newA }


mirrorX : (a -> Bool) -> TransducerState a -> TransducerState a
mirrorX mapper state =
    if mapper state.initial then
        case state.scale of
            Just ( x, y ) ->
                { state | scale = Just ( -1 * x, y ) }

            Nothing ->
                { state | scale = Just ( -1, 1 ) }
    else
        state


mirrorY : (a -> Bool) -> TransducerState a -> TransducerState a
mirrorY mapper state =
    if mapper state.initial then
        case state.scale of
            Just ( x, y ) ->
                { state | scale = Just ( x, -1 * y ) }

            Nothing ->
                { state | scale = Just ( 1, -1 ) }
    else
        state

scale : (a -> ( Float, Float )) -> TransducerState a -> TransducerState a
scale mapper state =
    let
        ( newX, newY ) =
            mapper state.initial
    in
    case state.scale of
        Just ( x, y ) ->
            { state | scale = Just ( newX * x, newY * y ) }

        Nothing ->
            { state | scale = Just ( newX, newY ) }


attributes : TransducerState a -> List (Attribute Msg)
attributes state =
    let
        lengthToString =
            Pixels.toFloat >> String.fromFloat
    in
    [ state.translation |> Maybe.map (Tuple.mapBoth lengthToString lengthToString >> (\( x, y ) -> "translate(" ++ x ++ "px, " ++ y ++ "px)"))
    , state.scale |> Maybe.map (Tuple.mapBoth String.fromFloat String.fromFloat >> (\( x, y ) -> "scale(" ++ x ++ ", " ++ y ++ ")"))
    , state.rotation |> Maybe.map (\a -> "rotate(" ++ (Angle.inDegrees a |> String.fromFloat) ++ "deg)")
    ]
        |> List.filterMap identity
        |> String.join " "
        |> Tuple.pair "transform"
        |> uncurry Hats.style
        |> List.singleton
