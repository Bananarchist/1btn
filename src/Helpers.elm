module Helpers exposing (..)

import Quantity exposing (Quantity)


duple : a -> ( a, a )
duple a =
    ( a, a )


flip : (a -> b -> c) -> b -> a -> c
flip fn p2 p1 =
    fn p1 p2


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( p1, p2 ) =
    fn p1 p2


curry : (( a, b ) -> c) -> a -> b -> c
curry fn p1 p2 =
    fn ( p1, p2 )

defaultTo : a -> Bool -> a -> a 
defaultTo default test returnThis =
    if test then returnThis else default

dupleInto : (b -> c -> d) -> (( a, a ) -> ( b, c )) -> a -> d
dupleInto fn maps =
    duple
        >> maps
        >> uncurry fn


doubleSpicyCurry : (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
doubleSpicyCurry fn firstMap secondMap =
    duple
        >> Tuple.mapBoth firstMap secondMap
        >> uncurry fn


mapEach : (a -> b) -> ( a, a ) -> ( b, b )
mapEach fn =
    Tuple.mapBoth fn fn


liftTuple : ( Maybe a, Maybe b ) -> Maybe ( a, b )
liftTuple val =
    case val of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing


maybeUncurry : (a -> b -> c) -> ( Maybe a, Maybe b ) -> Maybe c
maybeUncurry fn =
    liftTuple >> Maybe.map (uncurry fn)


{-| Return True if the third parameter is between the first and second
-}
within : comparable -> comparable -> comparable -> Bool
within min max value =
    min <= value && value <= max


{-| Exclusive variant of within
-}
withex : comparable -> comparable -> comparable -> Bool
withex min max value =
    min < value && value < max


{-| Within for Elm-Units
-}
between : Quantity number units -> Quantity number units -> Quantity number units -> Bool
between min max value =
    Quantity.lessThanOrEqualTo max value
        && Quantity.greaterThanOrEqualTo min value


{-| Withex for Elm-Units
-}
betwex : Quantity number units -> Quantity number units -> Quantity number units -> Bool
betwex min max value =
    Quantity.lessThan max value
        && Quantity.greaterThan min value


{-| Clamps to nearest, rounding down in case of tie

    nearest (Angle.degrees 10) (Angle.degrees 30) (Angle.degrees 25) == Angle.degrees 30

-}
nearest : Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units
nearest a b test =
    let
        min =
            Quantity.min a b

        max =
            Quantity.max a b

        mid =
            Quantity.midpoint min max
    in
    if Quantity.lessThan min test then
        min

    else if Quantity.greaterThan max test then
        max

    else if Quantity.greaterThan mid test then
        max

    else
        min


{-| Tests if all predicates applied to a value return True, returning
False as soon as it is encountered and ignoring remaining tests
-}
allPredicates : List (a -> Bool) -> a -> Bool
allPredicates fns val =
    let
        -- define iterator for short circuiting
        listIter predicates param last =
            case ( predicates, last ) of
                ( _, False ) ->
                    False

                ( p :: ps, True ) ->
                    listIter ps param (p param)

                ( [], _ ) ->
                    last
    in
    listIter fns val True


{-| Tests all predicates to assure at least one returns True
-}
anyPredicates : List (a -> Bool) -> a -> Bool
anyPredicates fns val =
    List.foldl ((|>) val >> (||)) False fns



--(\fn acc -> fn val || acc) False fns


{-| Takes a list of functions to apply to a value, from the left, returning a list
-}
buildl : List (a -> List b) -> a -> List b
buildl fns val =
    List.foldl ((|>) val >> (++)) [] fns


{-| Takes a list of functions to apply to a value, from the right, returning a list
-}
buildr : List (a -> List b) -> a -> List b
buildr fns val =
    List.foldr ((|>) val >> (++)) [] fns


{-| Takes a list of functions and mapping functions and applies them in a series
process
-}
process : List ( a -> b, b -> a -> a ) -> a -> a
process fns val =
    List.foldl (\( map, acc ) result -> acc (map result) result) val fns


{-| Place value at end of list if test passes
-}
appendIf : Bool -> a -> List a -> List a
appendIf test val list =
    if test then
        list ++ [ val ]

    else
        list


{-| Place value at front of list if test passes
-}
consIf : Bool -> a -> List a -> List a
consIf test val list =
    if test then
        val :: list

    else
        list


concatIf : Bool -> List a -> List a -> List a
concatIf test vals list =
    if test then
        list ++ vals

    else
        list
