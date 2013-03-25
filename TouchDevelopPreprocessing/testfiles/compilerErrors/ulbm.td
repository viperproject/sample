meta version "v2.2,nothing";
meta name "App I";
meta color "#ff007FFF";
//
meta platform "current";

action main() {
  "Hello world!" →post_to_wall;
  $s := "big boi";
  $s2 := "no problem";
  $s2→post_to_wall;
  $x := wall→ask_number("Multiply n by pi. n = input");
  code→go($x)→post_to_wall;
  $s→post_to_wall;
  art→pic→post_to_wall;
}

action show() {
  if box→is_init then {
    $s := "My name is Ali";
  }
  if true then {
    skip;
  }
}

action go(p: Number) returns r: Number {
  $r := $p * math→\u03C0;
  meta private;
}
var pic : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/vbanghzo";
}

event tap_wall_Place(item: Place) {
  art→pic→post_to_wall;
  meta private;
}
table thing {
  type = "Object";
}

