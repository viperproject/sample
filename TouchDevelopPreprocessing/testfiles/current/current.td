meta version "v2.2,js,ctx";
meta name "my script 2";
meta platform "current";

action main() {
  $v := collections→create_number_collection;
  $v→add(1);
  $v→add(2);
  $x := $v→at(0);
  $y := $v→at(1);
  if $x = $y then {
    "yes" →post_to_wall;
  }
  else {
    "no" →post_to_wall;
  }
}

action go() {
  $v := collections→create_number_collection;
  for 0 ≤ i < 2 do {
    $v→add($i + 1);
  }
  $x := $v→at(0);
  $y := $v→at(1);
  if $x = $y then {
    "yes" →post_to_wall;
  }
  else {
    "no" →post_to_wall;
  }
}