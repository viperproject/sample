meta version "v2.2,nothing";
meta name "my script";
meta color "#ffdaa520";
// Record what you say when you shake your phone
meta platform "current";

action main() {
  media→songs→play;
}

event shake() {
  senses→record_microphone→play;
  meta private;
}
table thing {
  type = "Decorator";
  keys {
    target: Sprite
  }
}

