meta version "v2.2,nothing";
meta name "Gamex";
// Tap the screen to creates bubbles!
meta platform "current";

action main() {
  // The board is a 2D canvas with a physics engine. Gravity makes sprites fall and boundary makes them bounce.
  wall→set_background(colors→random);
  data→Gamex_2\u002E0 := media→create_landscape_board(800, 480);
  data→Gamex_2\u002E0→set_gravity(0, 2000);
  data→Gamex_2\u002E0→create_boundary(0);
  $info := data→Gamex_2\u002E0→create_text(100, 20, 24, "tap to create bubbles");
  $info→set_friction(1);
  // Display the board. The game loop will start after leaving main.
  data→Gamex_2\u002E0→post_to_wall;
}
var Gamex_2\u002E0 : Board {
}

event gameloop() {
  // Apply physics and redraw.
  data→Gamex_2\u002E0→evolve;
  data→Gamex_2\u002E0→update_on_wall;
  meta private;
}

event tap_board\u003A_Gamex_2\u002E0(x: Number, y: Number) {
  // When the board is tapped, create a circle sprite that falls down.
  $circle := data→Gamex_2\u002E0→create_ellipse(100, 100);
  $circle→set_pos($x, $y);
  $circle→set_color(colors→random);
  $circle→set_friction(0.007);
  meta private;
}
var pic : Picture {
  is\_resource = true;
}
table thing {
  type = "Object";
}

