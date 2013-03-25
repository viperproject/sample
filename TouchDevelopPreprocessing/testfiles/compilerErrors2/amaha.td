meta version "v2.2,nothing";
meta name "Binary Post";
meta icon "bolt";
meta color "#ff00008b";
//
meta platform "current";

action main() {
  $s := wall→ask_string("Input Message");
  $bin := "";
  foreach s1 in $s where true do {
    $n1 := $s1→to_unicode;
    $x := 128;
    while $x > 0 do {
      if $n1 ≥ $x then {
        $bin := $bin→concat("1");
        $n1 := $n1 - $x;
      }
      else {
        $bin := $bin→concat("0");
      }
      $x := math→floor($x / 2);
    }
    $bin := $bin→concat(" ");
  }
  $bin→share("social");
}

