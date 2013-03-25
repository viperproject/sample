meta version "v2.2,nothing";
meta name "nearCam";
meta icon "bullseye";
meta color "#ff85bb65";
// Must use flash to work!
meta platform "current";

action main() {
  while true do {
    $pic := senses→take_camera_picture;
    $pic→colorize(colors→background, colors→foreground, 0.41);
    $pic→post_to_wall;
    $b := wall→ask_boolean("Do you want to save to your phone?", "");
    if $b→equals(true) then {
      $pic→save_to_library;
    }
    wall→clear;
  }
}

