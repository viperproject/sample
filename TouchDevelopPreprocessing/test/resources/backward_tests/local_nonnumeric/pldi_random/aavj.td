// Make up anagrams for people you know!
//:: ExpectedOutput(backward.counterexample.found)
action main() {
  wall→prompt("Select a contact then move letters around to make anagram\n\nTap to save snapshot");
  $chosen_contact := social→choose_contact;
  //:: ExpectedOutput(assert.failed)
  $s2 := $chosen_contact→name;
  skip;
  data→board := media→create_portrait_board(480, 800);
  data→sprite_set := data→board→create_sprite_set;
  $len := $s2→count;
  $font := 100;
  $size := 100;
  for 0 ≤ i < $len do {
    $c := $s2→substring($i, 1);
    if $c→equals(" ") or $c→equals(",") then {
      skip;
    }
    else {
      data→sprite := data→board→create_text($size, $size, $font, $c);
      data→sprite→set_color(colors→white);
      data→sprite→set_pos($size + math→mod($i, 4) * $size, $size + $size * math→floor($i / 4));
      data→sprite_set→add(data→sprite);
    }
  }
  data→board→post_to_wall;
}

var board : Board {
  transient = true;
}

event gameloop() {
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}

event swipe_board\u003a_board(x: Number, y: Number, delta_x: Number, delta_y: Number) {
  skip;
  meta private;
}

var sprite_set : Sprite_Set {
  transient = true;
}

var sprite : Sprite {
  transient = true;
}

event swipe_sprite_in_sprite_set(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  $sprite→move($delta_x, $delta_y);
  meta private;
}

var sprite3 : Sprite {
  transient = true;
}

event tap_board\u003a_board(x: Number, y: Number) {
  $b := wall→ask_boolean("Save snapshot?", "");
  if $b then {
    wall→clear;
    data→board→post_to_wall;
    $pic := wall→screenshot;
    $pic→share("social", "");
  }
  else {
    wall→clear;
    data→board→post_to_wall;
  }
  meta private;
}
