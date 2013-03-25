meta version "v2.2,nothing";
meta name "DetailCaprure";
meta icon "shrink";
meta color "#ff66ff00";
//
meta platform "current";

action main() {
  $pic := senses→take_camera_picture;
  $pic→colorize(colors→background, colors→foreground, 0.40);
  $pic→post_to_wall;
  $b := wall→ask_boolean("Do you want save? ", "");
  if $b→equals(true) then {
    $pic→save_to_library;
    wall→clear;
  }
  else {
    $pic→clear(colors→background);
    wall→clear;
  }
}

