meta version "v2.2,nothing";
meta name "象棋";
// Tap the screen to creates bubbles!
meta platform "current";

action main() {
  // The board is a 2D canvas with a physics engine. Gravity makes sprites fall and boundary makes them bounce.
  wall→set_background(colors→random);
  data→in\_air := true;
  data→board := media→create_landscape_board(800, 480);
  data→board→set_gravity(0, 999);
  data→board→create_obstacle( - 400, 470, 8000, 0, 1);
  data→obstacles := data→board→create_sprite_set;
  $info := data→board→create_text(100, 20, 24, "tap to create bubbles");
  $info→set_friction(1);
  // Display the board. The game loop will start after leaving main.
  data→board→post_to_wall;
}
var board : Board {
}

event gameloop() {
  // Apply physics and redraw.
  data→board→evolve;
  code→update\_boxes;
  if data→runner→y ≠ 460 then {
    if data→in\_air then {
      data→runner→set_elasticity(0);
      data→in\_air := false;
    }
  }
  if data→runner→y = 460 then {
    data→runner→set_speed_x(0);
  }
  data→board→update_on_wall;
  meta private;
}

event tap_board\u003A_board(x: Number, y: Number) {
  // When the board is tapped, create a circle sprite that falls down.
  if data→board→count < 4 then {
    data→runner := data→board→create_rectangle(20, 20);
    data→runner→set_pos(200, 300);
    data→runner→set_color(colors→black);
    data→runner→set_speed(30, 0);
    data→runner→set_elasticity(0);
  }
  else {
    if data→runner→y = 460 then {
      data→runner→set_speed_y(500);
      data→runner→set_elasticity(1);
      data→in\_air := true;
    }
  }
  meta private;
}
var runner : Sprite {
}
var obstacles : Sprite_Set {
}
var done\_moving : Boolean {
}
var in\_air : Boolean {
}

action update\_boxes() {
  if data→board→count > 1 and data→obstacles→count < 2 then {
    data→obstacles→add(data→board→create_rectangle(20, 20));
    data→obstacles→add(data→board→create_rectangle(20, 20));
    foreach e in data→obstacles where true do {
      $e→set_pos(800 + data→obstacles→index_of($e) * 100, 460);
      $e→set_speed_x( - 30);
    }
  }
  else {
    foreach e1 in data→obstacles where $e1→x < 0 do {
      $e1→set_pos(math→abs(data→obstacles→at(0)→x) + 800, 460);
      $e1→set_speed_x( - 30);
    }
  }
  skip;
  meta private;
}

