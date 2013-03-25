meta version "v2.2,nothing";
meta name "voice modulator (first)";
//
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

event phone_face_up() {
  collections;
  wall→set_background(colors→blue);
  while senses→has_gyroscope→equals(false) do {
    "Hello please set pitch." →post_to_wall;
    senses→record_microphone→play_special(1, 1, 1);
  }
  meta private;
}
var sound : Sound {
  is\_resource = true;
}
var c : Color {
  is\_resource = true;
  url = "#ff275387";
}

