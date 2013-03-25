meta version "v2.2,nothing";
meta name "L-System";
meta icon "pencil";
//
meta platform "current";
meta import turtle {
  pub "wzhc"
  usage {
    action initialize()
    action pen_down()
    action move(distance: Number)
    action turn(angle: Number)
  }
}

action main(Levels: Number, Angle: Number, Distance: Number, Seed: String, Generator: String) {
  $s := $Seed;
  for 0 ≤ i < $Levels do {
    $s := $s→replace("-", $Generator);
  }
  ♻ turtle→initialize;
  ♻ turtle→pen_down;
  foreach s1 in $s where true do {
    if $s1→equals("-") then {
      ♻ turtle→move($Distance);
      ... ;
    }
    else {
      if $s1→equals("L") then {
        ♻ turtle→turn($Angle);
      }
      else {
        if $s1→equals("R") then {
          ♻ turtle→turn( - $Angle);
        }
      }
    }
  }
}

