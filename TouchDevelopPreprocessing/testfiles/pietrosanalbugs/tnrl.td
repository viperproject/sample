meta version "v2.2,nothing";
meta name "a wittle wabbit";
//
meta platform "current";

action main() {
  data→board := media→create_board(640);
  data→turtles := data→board→create_sprite_set;
  data→rabbit := data→board→create_ellipse(20, 20);
  data→target := data→board→create_ellipse(20, 20);
  data→target→set_pos(200, 200);
  data→target→set_color(colors→blue);
  data→target→hide;
  data→board→set_background(colors→black);
  data→board→post_to_wall;
}

event gameloop() {
  $p := data→board→touch_current;
  if not math→is_nan($p→x) and not math→is_nan($p→y) then {
    data→target→set_pos($p→x, $p→y);
  }
  data→rabbit→speed_towards(data→target, 250);
  code→update_turtles;
  code→collide_with_turtles\u003F;
  code→spawn_turtles;
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}
var board : Board {
}
var rabbit : Sprite {
}
var target : Sprite {
}
var turtles : Sprite_Set {
}

action update_turtles() {
  foreach e in data→turtles where true do {
    $e→speed_towards(data→rabbit, 100);
  }
  meta private;
}

action collide_with_turtles\u003F() {
  $collide := data→rabbit→overlap_with(data→turtles);
  if $collide→count > 0 then {
    data→rabbit→set_color(colors→red);
    data→board→update_on_wall;
    time→stop;
  }
  meta private;
}

action spawn_turtles() {
  if math→random(100) < 5 then {
    $sprite := data→board→create_rectangle(20, 20);
    $sprite→set_angle(45);
    $sprite→set_color(colors→random);
    $sprite→set_pos(math→random(data→board→width), math→random(data→board→height));
    data→turtles→add($sprite);
  }
  meta private;
}

