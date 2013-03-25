meta version "v2.2,nothing";
meta name "try cool stuff";
meta icon "SmartPhone";
meta color "#ffb7410e";
//
meta platform "current";

action beatbox() {
  wall→prompt("Welcome to the TouchStudio beat box!\n\nStart by recording 2 beat sounds that you will be able to play later. TouchStudio will pick a random song from your media library and play it in the background.\n\nReady to start recording?");
  $sound := senses→record_microphone;
  $sound→set_pitch( - 0.2);
  $sound→post_to_wall;
  wall→prompt("Record another beat sound");
  $sound1 := senses→record_microphone;
  $sound1→set_pitch(0.2);
  $sound1→post_to_wall;
  $pic := media→create_picture(480, 480);
  $pic→clear(colors→white);
  $pic→post_to_wall;
  $song := media→songs→at(math→rand(media→songs→count));
  player→play($song);
  $pics := media→pictures;
  while true do {
    time→sleep(0.1);
    $pic→fill_rect(0, 0, $pic→width, $pic→height, 0, colors→from_argb(0.2, 1, 1, 1));
    $thumb := $pics→thumbnail(math→rand($pics→count));
    $pic→blend($thumb, 12 + math→rand(480 - 24 - $thumb→width), 12 + math→rand(480 - 24 - $thumb→height), math→rand_norm * 60 - 30, 1);
    $pic→update_on_wall();
  }
}

action vibrato_morse() {
  $s1 := wall→ask_string("Translate to Morse code");
  $strings := ".-!-...!-.-.!-..!.!..-.!--.!....!..!.---!-.-!.-..!--!-.!---!.--.!--.-!.-.!...!-!..-!...-!.--!-..-!-.--!--.." →split("!");
  $x := "a" →at(0);
  foreach s2 in $s1 where true do {
    code→extracted($x, $strings);
    time→sleep(0.5);
  }
}

action themify_picture() {
  wall→prompt("Pick a picture to themify");
  $pic := media→choose_picture;
  $pic→post_to_wall;
  $pic→colorize(colors→accent, colors→background, 0.65);
  $pic→update_on_wall;
  if wall→ask_boolean("Do you want to save this picture?", "Once it is saved, you can go to the \'Saved picturespictures album and set it as your wallpaper...") then {
    $pic→save_to_library;
  }
}

action extracted(x: String, strings: String_Collection) {
  ... ;
  meta private;
}

