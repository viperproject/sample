meta version "v2.2,nothing";
meta name "turtle o meter";
meta icon "snowflake";
meta color "#ff007fff";
// Move the turtle using the accelerometer...
meta platform "current";
meta import t {
  pub "sicm"
  usage {
    action initialize()
    action forward(distance: Number)
    action set_turtle_angle(angle: Number)
  }
}

action main() {
  // Edit this action to create your own turtle drawings.
  ♻ t→initialize;
  // Change the code below to draw your own patterns.
  while true do {
    // Get the acceleration and convert it into an angle
    $p := senses→acceleration_smooth;
    $a := math→atan2($p→y, $p→x);
    // a is in radians. Convert it to degrees!
    $x := math→rad_to_deg($a);
    ♻ t→set_turtle_angle($x);
    // Move the turtle
    ♻ t→forward(10);
  }
}

