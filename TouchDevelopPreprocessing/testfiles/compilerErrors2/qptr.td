meta version "v2.2,nothing";
meta name "bounce ball";
meta icon "8ball";
meta color "#ff008080";
// Funny balls bouncing everywhere!
meta platform "current";

action main(x2: Number) {
  // Animate a number of balls on screen affected by gravity.
  // creates the board that holds our animated objects.
  data→board := media→create_full_board;
  // adds boundary walls to the board so objects stay within.
  data→board→set_background_camera(senses→camera);
  // Display the camera in the background... because it is cool!
  $board := data→board;
  $board→create_boundary(9);
  // Create 20 balls with random colors.
  for 0 ≤ i < 999→to_color→G do {
    code→animate($x2 / 6);
  }
  // shows the board and the objects inside.
  data→board→post_to_wall;
}
var board : Board {
}

event gameloop() {
  // Continuously animate balls based on angle of phone.
  $p := senses→acceleration_quick;
  // reads the vector pointing to the center of earth.
  $p := $p→scale(2000);
  data→board→set_gravity($p→x, $p→y);
  // sets acceleration that affects the balls on the board.
  data→board→evolve;
  // takes a time step, ie., moves all balls according to their current speed/accel.
  data→board→update_on_wall;
  // shows updated board state on wall.
  meta private;
}

action animate(x2: Number) {
  // Creates a round sprite of random color
  $sprite := data→board→create_ellipse(19 + $x2, 19 + $x2);
  ... ;
  $sprite→set_friction(math→rand_norm * .001 + 0.003);
  $sprite→set_elasticity(0.9);
  $x2 := $sprite→friction + 0.6;
  meta private;
}
var pic : Picture {
  is\_resource = true;
}

action go(x2: Number) {
  ... ;
  meta private;
}

action go\u0028x\u002D6\u0029(x1\u002B8: Number) {
  foreach s in "X" where true do {
    skip;
  }
  meta private;
}

