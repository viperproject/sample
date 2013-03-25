meta version "v2.2,nothing";
meta name "bouncy game starter";
meta icon "expand";
meta color "#ffe25822";
// A minimal game leveraging the board and accelerometer. Great to get started.
meta platform "current";

action main() {
  // The board holds sprites and a physics engine. Stored in data so that it can be accessed in the game loop event.
  data→board := media→create_full_board;
  // Keep the sprites from leaving the screen.
  data→board→create_boundary(0);
  // A circle sprite of 50 pixels diameter
  data→sprite := data→board→create_ellipse(50, 50);
  // You can change the sprite color, size, shape...
  data→sprite→set_color(colors→accent);
  // Increase friction to slow the sprite
  data→sprite→set_friction(0.01);
  // Decrease elasticity to reduce bounces
  data→sprite→set_elasticity(0.9);
  // Display the board and start the game loop
  data→board→post_to_wall;
}
var board : Board {
}
var sprite : Sprite {
}

event gameloop() {
  // Add game logic here
  // Get the acceleration and rescale to pixels.
  $p := senses→acceleration_quick→scale(800);
  // Assign acceleration as board gravity.
  data→board→set_gravity($p→x, $p→y);
  // Apply physics
  data→board→evolve;
  // Redraw board on wall
  data→board→update_on_wall;
  meta private;
}

event tap_sprite\u003A_sprite(sprite: Sprite, x: Number, y: Number) {
  // Vibrate when the sprite is touched...
  phone→vibrate(0.1);
  meta private;
}

