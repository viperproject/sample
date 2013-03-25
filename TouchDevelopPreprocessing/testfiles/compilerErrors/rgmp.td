meta version "v2.2,nothing";
meta name "Othello";
meta icon "ninecolumn";
meta color "#ff85bb65";
//
meta platform "current";

action main() {
  data→bord := media→create_board(640);
  data→bord→set_background(art→c);
  data→bord→post_to_wall;
  code→TekenGrid;
  code→FillGrid;
}
var bord : Board {
}
var GridMap : Number_Map {
}

action TekenGrid() {
  data→pic := media→create_picture(400, 400);
  data→rooster := data→bord→create_picture(data→pic);
  for 0 ≤ i < 9 do {
    data→pic→draw_line(0, $i * 50, 400, $i * 50, colors→blue, 3);
    data→pic→draw_line($i * 50, 0, $i * 50, 400, colors→blue, 3);
  }
  data→bord→update_on_wall;
  meta private;
}

action CreateNumberMap() {
  data→GridMap := collections→create_number_map;
  // teken grid en zet initiële waarden
  for 0 ≤ i < 64 do {
    $index := $i + 1;
    data→GridMap→set_at($index, 0);
  }
  data→GridMap→set_at(28, 1);
  data→GridMap→set_at(29, 2);
  data→GridMap→set_at(37, 1);
  data→GridMap→set_at(36, 2);
  meta private;
}
table Velden {
  type = "Table";
  fields {
    x: Number_field
    Kol: String_field
    Row: Number_field
  }
}
var c : Color {
  is\_resource = true;
  url = "#ffbfbb7e";
}
var pic : Picture {
}
var rooster : Sprite {
}
var schf : Sprite {
}
var GroenRood : Sprite_Set {
  // de begin kleuren
}

action FillGrid() {
  data→Schijven := data→bord→create_sprite_set;
  data→GroenRood := data→bord→create_sprite_set;
  $RSchf := data→bord→create_ellipse(20, 20);
  $RSchf→set_pos(50, 50);
  $RSchf→set_color(colors→red);
  data→GroenRood→add($RSchf);
  $GSchf := data→bord→create_ellipse(20, 20);
  $GSchf→set_pos(150, 50);
  $GSchf→set_color(colors→green);
  data→GroenRood→add($GSchf);
  for 0 ≤ i < 64 do {
    $i1 := $i + 1;
    data→schf := data→bord→create_ellipse(40, 40);
    data→schf→set_pos((math→mod($i1, 8.001) * 50) + 3, (math→floor($i1 / 8.001) * 50) + 147);
    data→schf→set_color(colors→white);
    data→Schijven→add(data→schf);
  }
  data→Schijven→at(27)→set_color(colors→green);
  data→Schijven→at(28)→set_color(colors→red);
  data→Schijven→at(35)→set_color(colors→red);
  data→Schijven→at(36)→set_color(colors→green);
  data→bord→update_on_wall;
  meta private;
}
var Schijven : Sprite_Set {
}

event tap_sprite_in_Schijven(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  $sprite→set_color(data→Kleur);
  $sprite→set_opacity(1);
  data→bord→update_on_wall;
  meta private;
}

event tap_sprite_in_GroenRood(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $index_in_set = 1 then {
    data→Kleur := colors→green;
  }
  else {
    data→Kleur := colors→red;
  }
  meta private;
}
var Kleur : Color {
}

