module ControlsTests exposing (suite)

import Expect
import Test exposing (Test, test, describe, fuzz3, fuzz)
import Fuzz 
import Model
import Quantity
import Duration

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
      |> Model.anyNotificationsか
      |> Expect.equal True
  , test "Game pad disconnected triggers notifications" <|
    \_ ->
      init
      |> Model.setListeningPad False
      |> Model.anyNotificationsか
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
  
  , fuzz (Fuzz.map Duration.milliseconds (Fuzz.floatRange 100 200)) "Held time accumulates" <|
    \dur ->
      let
        pressed = 
          init
          |> Model.setKeyboard True
        held =
          pressed
          |> Model.setCurrentTick dur
          |> Model.update
          |> Model.setLastTick dur
        released =
          held
          |> Model.setKeyboard False
        inactive =
          released
          |> Model.setCurrentTick dur
          |> Model.update

      in
      Expect.all
        [ Model.justPressedか pressed |> Expect.equal True |> Expect.onFail "Not pressed" |> always
        , Model.justPressedか held |> Expect.equal False |> Expect.onFail "Pressed instead of held" |> always
        , Model.anyPressedか inactive |> Expect.equal False |> Expect.onFail "Not inactive" |> always
        , Model.justReleasedか released |> Expect.equal True |> Expect.onFail "Not released" |> always
        ]
        dur
  ]
  |> describe "Controls tests"

