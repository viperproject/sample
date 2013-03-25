meta version "v2.2,nothing";
meta name "turtle triangle spiral";
meta icon "snowflake";
meta color "#ff007fff";
// A turtle program that draws triangles into a colored spiral.
meta platform "current";
meta import t {
  pub "neyh"
  usage {
    action initialize()
    action move(distance: Number)
    action turn(angle: Number)
    action pen_color(c: Color)
  }
}

action main() {
  // Edit this action to create your own turtle drawings.
  ♻ t→initialize;
  // Change the code below to draw your own patterns.
  $length := 100;
  for 0 ≤ i < 64 do {
    // Turn a little and draw a triangle
    ♻ t→turn(360 / 64);
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
    ♻ t→move($length);
    // then turn to the left
    ♻ t→turn(360 / 3);
  }
  meta private;
}

action spiral_color(i: Number) {
  ♻ t→pen_color(colors→from_hsb($i, 0.6, 0.6));
  meta private;
}

