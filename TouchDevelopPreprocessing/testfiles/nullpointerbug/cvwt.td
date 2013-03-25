meta version "v2.2,nothing";
meta name "ShakeMe";
meta color "#ffffa500";
// Shanks the phone to random play music.
meta platform "current";

action main() {
  "Face your phone up to resume " →post_to_wall;
  "Face your phone down to pause " →post_to_wall;
  "Shake your phone to play music " →post_to_wall;
  code→PlaySong;
}

event shake() {
  code→PlaySong;
  meta private;
}

event phone_face_down() {
  player→pause;
  meta private;
}

event phone_face_up() {
  $p := player→is_paused;
  if $p→equals(true) then {
    player→resume;
  }
  else {
    code→PlaySong;
  }
  meta private;
}

action PlaySong() {
  media→songs→random→play;
  meta private;
}

