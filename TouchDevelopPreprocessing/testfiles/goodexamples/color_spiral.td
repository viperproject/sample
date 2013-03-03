meta version "v2.2,nothing";
meta name "turtle triangle spiral";
meta icon "snowflake";
meta color "#ff007fff";
// A turtle program that draws triangles into a colored spiral.
meta platform "current";
meta import turtle {
  pub "mzsi"
  usage {
    action initialize()
    action forward(distance: Number)
    action left_turn(angle: Number)
    action pen_color(c: Color)
  }
}

action main() {
  // Edit this action to create your own turtle drawings.
  ♻ turtle→initialize;
  // Change the code below to draw your own patterns.
  $length := 100;
  for 0 ≤ i < 64 do {
    // Turn a little and draw a triangle
    ♻ turtle→left_turn(360 / 64);
    // Increase size
    $length := $length + 2;
    // Update color
    code→spiral_color(($i + 1) / 64);
    // Draw a triangle.
    code→draw_triangle($length);
  }
  // Don't forget to publish your turtle drawing!!
}

action draw_triangle(length: Number) {
  // Draws a triangle
  for 0 ≤ i1 < 3 do {
    // On each step, move by 100 pixels
    ♻ turtle→forward($length);
    // then turn to the left
    ♻ turtle→left_turn(360 / 3);
  }
  meta private;
}

action spiral_color(i: Number) {
  ♻ turtle→pen_color(colors→from_hsb($i, 0.6, 0.6));
  meta private;
}
