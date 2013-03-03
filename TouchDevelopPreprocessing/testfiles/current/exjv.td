meta version "v2.2,nothing";
meta name "turtle";
meta icon "snowflake";
meta color "#ff007fff";
// A library to create turtle scripts
meta isLibrary "yes";
meta platform "current";
var board : Board {
}
var turtle : Sprite {
}
var world : Picture {
}

action initialize() {
  // Creates the trutle and wakes it up!
  data→board := media→create_board(640);
  // Create the turtle sprite
  data→turtle := data→board→create_ellipse(20, 10);
  data→turtle→set_color(colors→accent);
  data→turtle→set_pos(data→board→width / 2, data→board→height / 2);
  data→turtle→show;
  // Create the bitmap where the turtle will paint
  data→world := media→create_picture(data→board→width, data→board→height);
  data→world→clear(colors→background);
  data→board→set_background_picture(data→world);
  data→board→post_to_wall;
  data→thickness := 3;
  // Ready to paint!
  code→pen_down;
}
var drawing : Boolean {
}

action pen_down() {
  // Sets the pen down to enable the turtle to draw as it moves.
  data→drawing := true;
}

action pen_up() {
  // Lifts the pen up to stop drawing as the turtle moves.
  data→drawing := false;
}

action move_to(x: Number, y: Number) {
  // Turns and moves the turtle to the specified location. If the pen is down, it will draw a line as it moves.
  $tx := data→turtle→x;
  $ty := data→turtle→y;
  $wx, $wy := code→world_to_board($x, $y);
  $dx := $wx - $tx;
  $dy := $wy - $ty;
  $angle := math→atan2($dy, $dx);
  data→turtle→set_angle( - math→rad_to_deg($angle));
  if data→drawing then {
    $tc := data→turtle→color;
    data→world→draw_line($tx, $ty, $wx, $wy, $tc, data→thickness);
  }
  data→turtle→set_pos($wx, $wy);
  data→board→update_on_wall;
}

action move(distance: Number) {
  // Moves the turtle to a specified distance. If the pen is down, it will draw a line as it moves.
  $t := data→turtle;
  $tx := $t→x;
  $ty := $t→y;
  $angle := math→deg_to_rad( - $t→angle);
  $ex := $tx + $distance * math→cos($angle);
  $ey := $ty - $distance * math→sin($angle);
  if data→drawing then {
    $tc := data→turtle→color;
    data→world→draw_line($tx, $ty, $ex, $ey, $tc, data→thickness);
  }
  data→turtle→set_pos($ex, $ey);
  data→board→update_on_wall;
}

action turn(angle: Number) {
  // Turns the turtle by the specified angle. Angle is in degrees and can be either positive or negative. If the angle is positive, the turtle turns to its right. If it is negative, the turtle turns to its left.
  $x := data→turtle→angle;
  data→turtle→set_angle($x - $angle);
}

action turn_left() {
  // Turns 90 degrees to the left.
  code→turn(90);
}

action turn_right() {
  // Turns 90 degrees to the right.
  code→turn( - 90);
}

action pen_color(c: Color) {
  // Sets the color of the pen.
  data→turtle→set_color($c);
}

action world_to_board(x: Number, y: Number) returns bx: Number, by: Number {
  // Maps the world coordinates to the board coordinates.
  $bx := $x + data→board→width / 2;
  $by := data→board→height / 2 - $y;
  meta private;
}

action turtle_x() returns x: Number {
  // Returns the x coordinate of the turtle.
  $x := data→turtle→x - data→board→width / 2;
}

action turtle_y() returns y: Number {
  // Returns the y coordinate of the turtle.
  $y := data→board→height / 2 - data→turtle→y;
}

action turtle_angle() returns a: Number {
  // Returns the angle of the turtle.
  $a := 0 - data→turtle→angle;
}

action world_width() returns w: Number {
  // Returns the world width.
  $w := data→board→width;
}

action world_height() returns h: Number {
  // Returns the world height.
  $h := data→board→height;
}

action set_turtle_angle(angle: Number) {
  // Sets the turtle heading angle
  data→turtle→set_angle($angle);
}
var thickness : Number {
}

action set_thickness(thickness: Number) {
  // Sets the pen thickness in pixels
  data→thickness := math→max(1, $thickness);
}

