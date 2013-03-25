meta version "v2.2,nothing";
meta name "my script 4";
//
meta platform "current";

action main() {
  $hedef := locations→create_location(39.943039, 32.641384);
  $ls := maps→directions(senses→current_location, $hedef, false);
  $loc3 := locations→create_location(0, 0);
  $yol := 0;
  $b := true;
  foreach loc in $ls where true do {
    if $b then {
      $loc3 := $loc;
      $b := false;
    }
    $yol := $yol + $loc→distance($loc3);
    skip;
    $yol→post_to_wall;
    $loc3 := $loc;
    skip;
  }
  skip;
}

