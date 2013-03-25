meta version "v2.2,nothing";
meta name "Bitirme Projesi (Taksimetre)";
//
meta platform "current";

action main() {
  $strings := collections→create_string_collection;
  $strings→add("ankara");
  $strings→add("istanbul");
  $strings→add("izmir");
  $strings→add("eskisehir");
  $x := wall→pick_string("", "", $strings);
  wall→clear;
  $s := wall→ask_string("Gideceğiniz yeri giriniz");
  wall→clear;
  $hedef := locations→search_location($s, "", $strings→at($x), "Turkey");
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
    $loc3 := $loc;
    skip;
  }
  $yol→post_to_wall;
  "mesafe:" →post_to_wall;
  if $x = 0 then {
    $ucret := ($yol * 2.4) + 2.7;
    $ucret→post_to_wall;
  }
  else {
    if $x = 1 then {
      $ucret1 := ($yol * 1.7) + 2.7;
      $ucret1→post_to_wall;
    }
    else {
      if $x = 2 then {
        $ucret2 := ($yol * 2.25) + 2.5;
        $ucret2→post_to_wall;
      }
      else {
        if $x = 3 then {
          $ucret3 := ($yol * 2.5) + 2.5;
          $ucret3→post_to_wall;
        }
      }
    }
  }
  "ucret:" →post_to_wall;
  $ls→post_to_wall;
}

