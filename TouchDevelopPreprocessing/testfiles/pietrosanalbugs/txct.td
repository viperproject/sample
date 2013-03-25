meta version "v2.2,nothing";
meta name "flower";
// This flower is as nice as you
// Just enter your name and touch the screen
// Enjoy
meta platform "current";

action start() {
  $name := wall→ask_string("Your sweet name please ");
  if $name→is_invalid or $name→is_empty then {
    $name := "Flower";
  }
  ... ;
  $touch := 0;
  $length := $name→count;
  $s := "Kiss me " ∥ $length ∥ " times";
  ... ;
  wall→clear;
  wall→set_title($s);
  $board := media→create_board(640);
  $board→post_to_wall;
  $board→set_background_camera(senses→camera);
  ... ;
  while true do {
    if $board→touched and $touch < $length then {
      $display := $name→at($touch);
      $board→create_text(60, 60, 60, $display)→set_pos($board→touch_current→x, $board→touch_current→y);
      $x := $length - $touch - 1;
      $s1 := "Kiss Me " ∥ $x ∥ " times";
      wall→set_title($s1);
      $board→evolve;
      $board→update_on_wall;
      time→sleep(2);
      $touch := $touch + 1;
    }
    else {
      ... ;
      if $touch = $length then {
        wall→set_title("");
      }
      foreach sprite1 in $board where true do {
        $sprite1→set_pos(math→random(400), math→random(600));
        $board→update_on_wall;
      }
    }
    ... ;
  }
}

