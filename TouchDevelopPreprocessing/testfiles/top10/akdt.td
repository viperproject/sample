meta version "v2.2,nothing";
meta name "The Box 2";
meta icon "book";
meta color "#ffdaa520";
// Now faster... the first 3D demo on TouchDevelop.
meta platform "current";

action main() {
  $size := 200;
  $theta := math→\u03C0 / 15;
  $phi := math→\u03C0 / 5;
  $ys := math→create_number_map;
  $xs := math→create_number_map;
  $pic := media→create_picture(480, 600);
  $pic→post_to_wall;
  while true do {
    // Calculate coords for vertices
    for 0 ≤ i < 8 do {
      $p := code→toVector($i);
      $p2 := code→toScreen($theta, $p, $phi);
      $p2 := $p2→scale($size);
      $xs→set_at($i, $pic→width / 2 + $p2→x);
      $ys→set_at($i, $pic→height / 2 + $p2→y);
    }
    $pic→clear(colors→background);
    // Now draw the edges
    for 0 ≤ dimension < 3 do {
      // From and to store bitmasks for x,y,z coordinates in bits 0-2. Thanks for reading me, by the way.
      $from := 0;
      $to := 0;
      for 0 ≤ i2 < 4 do {
        $from, $to := code→getEdge($dimension, $i2);
        $pic→draw_line($xs→at($from), $ys→at($from), $xs→at($to), $ys→at($to), colors→accent, 1);
      }
    }
    $pic→update_on_wall;
    $theta := $theta + math→\u03C0 / 10;
    $phi := $phi + math→\u03C0 / 23;
  }
}

action toVector(from: Number) returns vec: Vector3 {
  $vec := math→create_vector3(math→mod($from, 2) - 0.5, math→mod(math→floor($from / 2), 2) - 0.5, math→floor($from / 4) - 0.5);
  meta private;
}

action toScreen(theta: Number, p: Vector3, phi: Number) returns p2: Vector3 {
  $x1 := math→sin($theta) * $p→x + math→cos($theta) * $p→y;
  $x2 := math→sin($theta) * $p→y - math→cos($theta) * $p→x;
  $y := math→cos($phi) * $p→z + math→sin($phi) * $x2;
  $p2 := math→create_vector3($x1, $y, 0);
  meta private;
}

action getEdge(dimension: Number, edge: Number) returns from: Number, to: Number {
  if $dimension = 0 then {
    $from := $edge * 2;
    $to := $from + 1;
  }
  else {
    if $dimension = 1 then {
      $from := math→floor($edge / 2) * 4 + math→mod($edge, 2);
      $to := $from + 2;
    }
    else {
      $from := $edge;
      $to := $from + 4;
    }
  }
  meta private;
}

