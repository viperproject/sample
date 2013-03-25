meta version "v2.2,nothing";
meta name "DanceParty";
meta icon "Help";
meta color "#ffFF00FF";
// Watch the Circles!!
meta platform "current";

action main() {
  // The board is a 2D canvas with a physics engine. Gravity makes sprites fall and boundary makes them bounce.
  data→board := media→create_landscape_board(800, 480);
  data→board→set_gravity( - 1000, 500);
  data→board→create_boundary(math→random(10));
  // Display the board. The game loop will start after leaving main.
  data→board→post_to_wall;
}
var board : Board {
}

event gameloop() {
  // Apply physics and redraw.
  wall→set_background(colors→random);
  data→board→evolve;
  data→board→set_background(colors→black);
  data→board→create_ellipse(10, 10)→set_color(colors→random);
  data→board→set_gravity(math→random(1000), math→random(1000));
  data→board→set_friction(0.00001);
  data→board→update_on_wall;
  meta private;
}

event tap_board\u003A_board(x: Number, y: Number) {
  skip;
  meta private;
}
table thing {
  type = "Object";
}

