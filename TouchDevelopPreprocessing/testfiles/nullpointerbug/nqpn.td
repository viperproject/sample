meta version "v2.2,nothing";
meta name "ShakeMusic";
//
meta platform "current,accelerometer,home,motion";

action main() {
  "甩歌吧!" →post_to_wall;
  code→PlaySong;
}

action PlaySong() {
  media→songs→random→play;
  meta private;
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

event tap_wall_String(item: String) {
  code→PlaySong;
  meta private;
}

