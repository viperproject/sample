meta version "v2.2,nothing";
meta name "snow";
meta icon "settings";
meta color "#ff007fff";
// Shake for snow!
meta platform "current";

action main() {
  code→init;
  wall→clear;
  data→board→post_to_wall;
}

action init() {
  data→nFlakes := 400;
  $minV := 1;
  $maxV := 20;
  $minH := - 20;
  $maxH := 20;
  $size := 5;
  $pic := media→choose_picture;
  if $pic→is_invalid then {
    time→stop;
  }
  if $pic→is_panorama then {
    data→board := media→create_landscape_board(800, 480);
  }
  else {
    data→board := media→create_portrait_board(480, 800);
  }
  data→board→set_background_picture($pic);
  data→sprite_set := data→board→create_sprite_set;
  for 0 ≤ i < data→nFlakes do {
    $sprite := data→board→create_ellipse($size, $size);
    $sprite→set_color(colors→white);
    $sprite→hide;
    $sprite→set_speed($minH + math→random_normalized * ($maxH - $minH), $minV + math→random_normalized * ($maxV - $minV));
    data→sprite_set→add($sprite);
  }
  meta private;
}
var nFlakes : Number {
}
var board : Board {
}
var sprite_set : Sprite_Set {
}

event shake() {
  for 0 ≤ i < data→nFlakes do {
    data→sprite_set→at($i)→set_pos(data→board→width * math→random_normalized, data→board→height * math→random_normalized);
    data→sprite_set→at($i)→show;
  }
  meta private;
}

event gameloop() {
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}

