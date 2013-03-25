meta version "v2.2,nothing";
meta name "LearnBraille";
meta icon "book";
meta color "#ff800080";
// Learn Braille
meta platform "current";

action main() {
  data→board := media→create_full_board;
  data→squares := data→board→create_sprite_set;
  code→generateSquares;
  $s := code→generateCircles();
  data→board→post_to_wall;
  $s→post_to_wall;
}
var board : Board {
  // board
}
var squares : Sprite_Set {
}

event tap_sprite_in_squares(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  $sprite→set_color(colors→random);
  phone→vibrate(0.2);
  data→board→update_on_wall;
  meta private;
}

action generateSquares() {
  $sprite11 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
  $sprite11→set_pos(data→board→width / 4, data→board→height / 6);
  $sprite12 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
  $sprite12→set_pos(data→board→width / 4 * 3, data→board→height / 6);
  $sprite21 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
  $sprite21→set_pos(data→board→width / 4, data→board→height / 6 * 3);
  $sprite22 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
  $sprite22→set_pos(data→board→width / 4 * 3, data→board→height / 6 * 3);
  $sprite31 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
  $sprite31→set_pos(data→board→width / 4, data→board→height / 6 * 5);
  $sprite32 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
  $sprite32→set_pos(data→board→width / 4 * 3, data→board→height / 6 * 5);
  data→board→set_background(colors→random);
  data→squares→add($sprite11);
  data→squares→add($sprite12);
  data→squares→add($sprite21);
  data→squares→add($sprite22);
  data→squares→add($sprite31);
  data→squares→add($sprite32);
  meta private;
}

action generateCircles() returns code1: String {
  $s := "";
  for 0 ≤ i < 6 do {
    $x := math→random(2);
    $s := $s→concat($x→to_string);
    if $x = 1 then {
      if $i = 1 then {
        $sprite11 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
        $sprite11→set_pos(data→board→width / 4, data→board→height / 6);
      }
      if $i = 2 then {
        $sprite12 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
        $sprite12→set_pos(data→board→width / 4 * 3, data→board→height / 6);
      }
      if $i = 3 then {
        $sprite21 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
        $sprite21→set_pos(data→board→width / 4, data→board→height / 6 * 3);
      }
      if $i = 4 then {
        $sprite22 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
        $sprite22→set_pos(data→board→width / 4 * 3, data→board→height / 6 * 3);
      }
      if $i = 5 then {
        $sprite31 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
        $sprite31→set_pos(data→board→width / 4, data→board→height / 6 * 5);
      }
      if $i = 6 then {
        $sprite32 := data→board→create_rectangle(data→board→width / 2, data→board→height / 3);
        $sprite32→set_pos(data→board→width / 4 * 3, data→board→height / 6 * 5);
      }
    }
  }
  foreach s1 in $s where $s1→to_number = 1 do {
    ... ;
  }
  $code1 := $s;
  meta private;
}

