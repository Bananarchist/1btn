module ViewTests exposing (suite)

import Expect
import Test exposing (Test, describe, fuzz, test, todo)
import Fuzz
import Test.Html.Query as Q
import Test.Html.Selector as Select
import Painter
import Game
import Duration
import Model


suite : Test
suite =
  [ test "Moth is in html" <|
    \_ -> 
      Model.init 600 500
      |> Game.view
      |> Q.fromHtml
      |> Q.find [ Select.id "moth" ]
      |> Q.has [ Select.id "moth" ]
  ]
  |> Test.concat
  |> List.singleton
  |> describe "View Tests"
