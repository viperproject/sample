meta version "v2.2,nothing";
meta name "Combined Gas Law Calculator";
meta color "#ffa52a2a";
// Simple to use calculator to find new volume, pressure or temperature using the combined gas law.
meta platform "current";

action main() {
  // The strips of variables are simply for the user to be able to input directly into the equations, this is a super simple code. But this is a good example for anyone wishing to learn how to use if then statements.
  $b := wall→ask_boolean("Find new pressure?", "");
  if $b→equals(true) then {
    $P := wall→ask_number("Original Pressure");
    $V := wall→ask_number("Original Volume");
    $T := wall→ask_number("Original Temperature");
    $B := wall→ask_number("New Volume");
    $C := wall→ask_number("New Temperature ");
    $x := ((($P * $V) / $T) * $C) / $B;
    $x→post_to_wall;
  }
  else {
    $b1 := wall→ask_boolean("Find New Volume?", "");
    if $b1→equals(true) then {
      $p := wall→ask_number("Original Pressure");
      $v := wall→ask_number("Original Volume");
      $t := wall→ask_number("Original Temperature");
      $a := wall→ask_number("New Pressure");
      $c := wall→ask_number("New Temperature ");
      $m := ((($p * $v) / $t) * $c) / $a;
      $m→post_to_wall;
    }
    else {
      $z := wall→ask_boolean("Find New Temperature?", "");
      if $z→equals(true) then {
        $p1 := wall→ask_number("Original Pressure");
        $v1 := wall→ask_number("Original Volume");
        $t1 := wall→ask_number("Original Temperature");
        $a1 := wall→ask_number("New Pressure");
        $B1 := wall→ask_number("New Volume");
        $G := ($a1 * $B1) / (($p1 * $v1) / $t1);
        $G→post_to_wall;
      }
    }
  }
}

