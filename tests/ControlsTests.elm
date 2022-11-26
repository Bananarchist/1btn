module ControlsTests exposing (suite)

import Expect
import Test exposing (Test, test, describe, fuzz3)
import Fuzz 
import Model

runXTimes : (a -> a) -> Int -> (a -> a)
runXTimes fn count =
  List.repeat count fn
  |> List.foldl (>>) identity

suite : Test
suite = 
  controls

init = Model.init 600 400 0

controls : Test
controls =
  [ test "Game pad connected triggers notifications" <|
    \_ -> 
      init
      |> Model.setListeningPad True
      |> Model.anyNotifications
      |> Expect.equal True
  , test "Game pad disconnected triggers notifications" <|
    \_ ->
      init
      |> Model.setListeningPad False
      |> Model.anyNotifications
      |> Expect.equal True
  , test "Second game pad disconnecting doesn't disable listening on pads" <|
    \_ ->
      init
      |> runXTimes (Model.setListeningPad True) 3
      |> Model.setListeningPad False
      |> Model.padCount
      |> Expect.equal 2
  , fuzz3 Fuzz.bool Fuzz.bool Fuzz.bool "Disjunctive true combinations of controls counts as anyPressed" <|
    \kbか mouseか gpか -> 
      init
      |> Model.setListeningPad True
      |> Model.setKeyboard kbか
      |> Model.setMouse mouseか
      |> Model.setPad gpか
      |> Model.anyPressedか
      |> Expect.equal (kbか || mouseか || gpか)
  , fuzz3 Fuzz.bool Fuzz.bool Fuzz.bool "If not listening, anyPressed is false" <|
    \kbか mouseか gpか -> 
      init
      |> Model.setListeningPad True
      |> Model.setKeyboard kbか
      |> Model.setMouse mouseか
      |> Model.setPad gpか
      |> Model.setListeningControls False
      |> Model.anyPressedか
      |> Expect.equal False
  ]
  |> describe "Controls tests"

