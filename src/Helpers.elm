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


{-| Return True if the third parameter is between the first and second -}
within: comparable -> comparable -> comparable -> Bool
within min max value =
    min < value && value < max

{-| Works with Elm-Units -}
between : Quantity number units -> Quantity number units -> Quantity number units -> Bool
between min max value =
    Quantity.lessThan max value
        && Quantity.greaterThan min value


{-| Tests is all predicates applied to a value return True, returning 
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

{-| Tests all predicates to assure at least one returns True -}
anyPredicates : List (a -> Bool) -> a -> Bool
anyPredicates fns val =
    List.foldl ((|>) val >> (||)) False fns  --(\fn acc -> fn val || acc) False fns

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
