meta version "v2.2,js,ctx";
meta name "Accent colors";
meta icon "raindrop";
meta color "#ffff00ff";
meta rootId "gPvTtyse4qaRT4qQHfgZeZvH";
meta isLibrary "yes";
meta platform "current";

action pick() returns(col: Color) {
  $cols := code→list_colors();
  $w := 75;
  $columns := 5;
  $board := media→create_board(480 / $columns * math→ceiling($cols→count / $columns));
  $W := $board→width / $columns;
  $tiles := $board→create_sprite_set;
  $count := 0;
  foreach s in $cols
  where true
  do {
    $css := $cols→at($s);
    $sprite := $board→create_rectangle($w, $w);
    $sprite→set_color($css→to_color);
    $sprite→set_pos($W * (math→mod($count, $columns) + .5), $W * (math→floor($count / $columns) + .5));
    $tiles→add($sprite);
    $count := $count + 1;
  }
  $touch := $board→create_ellipse(3, 23);
  $touch→set_opacity(0);
  $board→post_to_wall;
  //
  $touched_sprite := invalid→sprite;
  while $touched_sprite→is_invalid do {
    while not $board→touched do {
      time→sleep(.01);
    }
    $p := $board→touch_start;
    $touch→set_pos($p→x, $p→y);
    $ss := $touch→overlap_with($tiles);
    if $ss→count > 0 then {
      $touched_sprite := $ss→at(0);
    }
  }
  //
  $col := $touched_sprite→color;
}

action list_colors() returns(list: String_Map) {
  $list := collections→create_string_map;
  //
  $list→set_at("Lime", "ffA4c400");
  $list→set_at("Lime 7", "Ffa2c139");
  $list→set_at("Green", "Ff60a917");
  $list→set_at("Green 7", "Ff339933");
  $list→set_at("Emerald", "Ff008a00");
  $list→set_at("Teal", "Ff00aba9");
  $list→set_at("Cyan", "ff1ba1e2");
  $list→set_at("Cobalt", "Ff0050ef");
  $list→set_at("Indigo", "Ff6a00ff");
  $list→set_at("Violet", "Ffaa00ff");
  $list→set_at("Pink", "Fff472d0");
  $list→set_at("Pink 7", "Ffe671b8");
  $list→set_at("Magenta", "Ffd80073");
  $list→set_at("Crimson", "Ffa20025");
  $list→set_at("Red", "Ffe51400");
  $list→set_at("Orange", "Fffa6800");
  $list→set_at("Mango", "ffF09609");
  $list→set_at("Amber", "FfF0a30a");
  $list→set_at("Yellow", "Ffd8c100");
  $list→set_at("Brown", "Ff825a2c");
  $list→set_at("Brown 7", "Ffa05000");
  $list→set_at("Olive", "Ff6d8764");
  $list→set_at("Steel", "Ff647687");
  $list→set_at("Mauve", "Ff76608a");
  $list→set_at("Sienna", "Ff7a3b3f");
  meta private;
}

action show() {
  $cols := code→list_colors();
  foreach s in $cols
  where true
  do {
    $tb := wall→create_text_box($s, 36);
    $tb→set_background($cols→at($s)→to_color);
    $tb→set_foreground(colors→white);
    $tb→post_to_wall;
  }
}

action test() {
  $cols := code→list_colors();
  foreach s in $cols
  where true
  do {
    $cols→at($s)→to_color→post_to_wall;
  }
}

action names() returns(names: String_Collection) {
  $names := collections→create_string_collection;
  $cols := code→list_colors();
  foreach s in $cols
  where true
  do {
    $names→add($s);
  }
}

action by_name(name: String) returns(color: Color) {
  $color := invalid→color;
  $s := code→list_colors()→at($name);
  if not $s→is_invalid then {
    $color := $name→to_color;
  }
  meta private;
}