function anyButtonsPressed() {
  const pads = navigator.getGamepads();
  for (let pad of pads) {
    for (let btn of pad.buttons) {
      if (btn.pressed) {
        return true;
      }
    }
  }
  return false;
}

function manageGamePads(app) {
  var buttonPressed = false;

  window.addEventListener("gamepadconnected", (e) => {
    app.ports.padConnected.send(navigator.getGamepads().length || 0);
  });

  window.addEventListener("gamepaddisconnected", (e) => {
    app.ports.padDisconnected.send(navigator.getGamepads().length || 0);
  });

  app.ports.pollPadButtons.subscribe((msg) => {
    const anyPressed = anyButtonsPressed();
    const changed = anyPressed != buttonPressed;
    if (changed) {
      console.log(changed);
      if (buttonPressed) {
        app.ports.padButtonsDown.send(false);
      } else {
        app.ports.padButtonsDown.send(true);
      }
      buttonPressed = anyPressed;
    }
  });
}
