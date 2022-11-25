module First exposing (suite)

import Expect
import Test exposing (Test, test, describe, fuzz3, fuzz)
import Fuzz 
import Model


suite : Test
suite = 
  [ test "Initial game is not in game over" <|
    \_ ->
      Model.init 600 400
      |> Model.gameOverか
      |> Expect.equal False


  ]
  |> describe "Initial unstructured tests"

