/*
 * Load audio with XMLHttpRequest similar to other resources
 * Expect rsponseType of arraybuffer
 *
 * Decode using AudioContext
 */
const FartTone = "C#";

function fart(ctx) {
  const oscillator = new OscillatorNode(ctx, {
    type: "square",
    frequency: frequencyFor(FartTone, 1),
  });
  const gain = new GainNode(ctx, { gain: 0.1 });
  oscillator.connect(gain);
  gain.connect(ctx.destination);
  oscillator.start(ctx.currentTime);
  return oscillator; //oscillator.stop(ctx.currentTime + 0.2);
}

function manageAudio(app) {
  var ctx;
  var farting;
  app.ports.initializeAudioContext.subscribe(() => {
    ctx = new window.AudioContext();
  });
  app.ports.startFartSFX.subscribe(() => {
    /*if (!farting) {
      farting = fart(ctx);
    }*/
  });
  app.ports.stopFartSFX.subscribe(() => {
    if(farting) {
      farting.stop(ctx.currentTime);
      farting = false;
    }
  });
}

const noteMap = new Map();
noteMap.set("A", 27.5);
noteMap.set("A#", 29.14);
noteMap.set("Bb", 29.14);
noteMap.set("B", 30.87);
noteMap.set("C", 16.35);
noteMap.set("C#", 17.32);
noteMap.set("Db", 17.32);
noteMap.set("D", 18.35);
noteMap.set("D#", 19.45);
noteMap.set("Eb", 19.45);
noteMap.set("E", 20.6);
noteMap.set("F", 21.83);
noteMap.set("F#", 23.12);
noteMap.set("Gb", 23.12);
noteMap.set("G", 24.5);
noteMap.set("G#", 25.96);
noteMap.set("Ab", 25.96);

function frequencyFor(note, octave) {
  let nVal = noteMap.get(note) || 27.5;
  let fVal = octave < 0 ? nVal : nVal * octave;
  return fVal;
}
