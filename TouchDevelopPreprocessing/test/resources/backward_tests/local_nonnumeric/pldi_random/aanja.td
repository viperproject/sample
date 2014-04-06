action main() {
  // The board holds sprites and a physics engine. Stored in data so that it can be accessed in the game loop event.
  data→board := media→create_full_board;
  // Keep the sprites from leaving the screen.
  data→board→create_boundary(0);
  // A circle sprite of 50 pixels diameter
  skip;
  skip;
  skip;
  data→sprite := data→board→create_text(100, 100, 150, "💗");
  // You can change the sprite color, size, shape...
  data→sprite→set_color(colors→red);
  // Increase friction to slow the sprite
  skip;
  // Decrease elasticity to reduce bounces
  data→sprite→set_elasticity(1.0);
  // Display the board and start the game loop
  data→board→post_to_wall;
}

//:: ExpectedOutput(backward.counterexample.found)
event gameloop() {
  $acc := senses→acceleration_quick;
  //:: ExpectedOutput(assert.failed)
  $a := $acc→scale(200);
  data→board→set_gravity($a→x, $a→y);
  data→board→evolve;
  data→board→update_on_wall;
  // Add game logic here
  // Get the acceleration and rescale to pixels.
  $accp := senses→acceleration_quick;
  //:: ExpectedOutput(assert.failed)
  $p := $accp→scale(800);
  // Assign acceleration as board gravity.
  data→board→set_gravity($p→x, $p→y);
  // Apply physics
  data→board→evolve;
  // Redraw board on wall
  data→board→update_on_wall;
  meta private;
}

var board : Board {
  transient = true;
}

var sprite : Sprite {
  transient = true;
}

event tap_sprite\u003a_sprite(sprite: Sprite, x: Number, y: Number) {
  // Vibrate when the sprite is touched...
  phone→vibrate(0.1);
  $sprite→set_text("💔");
  phone→vibrate(0.1);
  skip;
  skip;
  meta private;
}

action physics() {
  $a := senses→acceleration_quick→scale(200);
  data→board→set_gravity($a→x, $a→y);
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}
