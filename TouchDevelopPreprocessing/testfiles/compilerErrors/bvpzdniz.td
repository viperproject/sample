meta version "v2.2,nothing";
meta name "my game";
meta icon "123";
meta color "#ffB7410E";
// Tap the screen to creates bubbles!
meta platform "current";

action main() {
  // The board is a 2D canvas with a physics engine. Gravity makes sprites fall and boundary makes them bounce.
  wall→set_background(colors→random);
  data→board := media→create_landscape_board(800, 480);
  data→board→set_gravity(0, 400);
  data→board→create_boundary(0);
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
  data→board→update_on_wall;
  meta private;
}

event tap_board\u003A_board(x: Number, y: Number) {
  // When the board is tapped, create a circle sprite that falls down.
  $circle := data→board→create_ellipse(100, 100);
  $circle→set_pos($x, $y);
  $circle→set_color(colors→random);
  $circle→set_friction(0.005);
  meta private;
}

action go() {
  skip;
  meta private;
}
var pic : Picture {
  is\_resource = true;
}
table thing {
  type = "Object";
  fields {
    f: Json_Object_field
  }
}

