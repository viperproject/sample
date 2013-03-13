meta version "v2.2,nothing";
meta name "Jogis erstes Spiel";
meta icon "Fire";
// Tap the screen to creates bubbles!
meta platform "current";

action main() {
  // The board is a 2D canvas with a physics engine. Gravity makes sprites fall and boundary makes them bounce.
  wall→set_background(colors→random);
  code→createBoard;
  data→sprites := data→board→create_sprite_set;
  $text := "tap to create bubbles";
  $info := data→board→create_text(100, 20, 24, $text);
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
  $b := false;
  foreach e in data→sprites where true do {
    skip;
    if $e→overlaps_with($circle) then {
      data→sprites→remove($e);
      $e→delete;
      $b := true;
    }
  }
  if $b then {
    $circle→delete;
  }
  else {
    data→sprites→add($circle);
  }
  meta private;
}

action createBoard() {
  data→board := media→create_landscape_board(800, 480);
  data→board→set_gravity(400, 400);
  data→board→create_boundary(0);
  meta private;
}
var sprites : Sprite_Set {
}

