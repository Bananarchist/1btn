module HelpersTests exposing (suite)

import Helpers
import Fuzz
import Expect
import Test exposing (Test, describe, fuzz2, fuzz3, test, todo)
import Length



suite : Test
suite =
  [ describe "Nearest" <|
    [ fuzz3 (Fuzz.floatRange 0 10) (Fuzz.floatRange -10 0) (Fuzz.floatAtLeast 11) "Returns nearest when positively outside range (nearest 4 7 10 == 7)" <|
      \a b t ->
        Helpers.nearest
          (Length.meters a)
          (Length.meters b)
          (Length.meters t)
        |> Length.inMeters
        |> Expect.within (Expect.Absolute 0.1) a
    , fuzz3 (Fuzz.floatRange 0 10) (Fuzz.floatRange -10 0) (Fuzz.floatAtMost -15) "Returns nearest when negatively outside range (nearest 4 7 1 == 4)" <|
      \a b t ->
        Helpers.nearest
          (Length.meters a)
          (Length.meters b)
          (Length.meters t)
        |> Length.inMeters
        |> Expect.within (Expect.Absolute 0.1) b
    , fuzz2 (Fuzz.floatRange 2 10) (Fuzz.floatRange -10 0 |> Fuzz.andThen(\f -> Fuzz.map (Tuple.pair f) (Fuzz.floatAtMost (f + 1)))) "Returns nearest when within range (nearest 4 7 5 == 4)" <|
      \a (b, t) ->
        Helpers.nearest
          (Length.meters a)
          (Length.meters b)
          (Length.meters t)
        |> Length.inMeters
        |> Expect.within (Expect.Absolute 0.1) b
    ]
  ]
  |> describe "Helpers tests"
