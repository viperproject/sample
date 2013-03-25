meta version "v2.2,nothing";
meta name "tic tac toe";
// Tap the screen to creates bubbles!
meta platform "current";

action main() {
  data→endgame := false;
  data→currentplayer := 1;
  data→board := media→create_portrait_board(480, 800);
  data→boxes := data→board→create_sprite_set;
  data→scoretext := data→board→create_sprite_set;
  data→scoreadded := true;
  data→resetscores := data→board→create_rectangle(20, 20);
  data→resetscores→set_pos(392, 718);
  data→numbers := collections→create_number_collection;
  for 0 ≤ l < 9 do {
    data→numbers→add(0);
  }
  $num := 0;
  for 0 ≤ j < 3 do {
    for 0 ≤ k < 3 do {
      data→boxes→add(data→board→create_rectangle(90, 90));
      data→boxes→at($num)→set_pos(85 + 155 * $j, 165 + 155 * $k);
      $num := $num + 1;
    }
  }
  for 0 ≤ i < 3 do {
    data→scoretext→add(data→board→create_text(100, 20, 60, ""));
    data→scoretext→at($i)→set_pos(50 + 80 * $i, 685);
  }
  data→scoretext→at(0)→set_color(colors→accent);
  data→scoretext→at(1)→set_color(colors→chrome);
  data→text := data→board→create_text(0, 0, 60, "");
  data→text→set_pos(15, 560);
  data→text→set_text("current player");
  data→board→post_to_wall;
}
var board : Board {
}

event gameloop() {
  foreach e in data→boxes where true do {
    $index := data→boxes→index_of($e);
    if $e→color→equals(colors→accent) then {
      data→numbers→set_at($index, 1);
      skip;
    }
    else {
      if $e→color→equals(colors→chrome) then {
        data→numbers→set_at($index, 5);
      }
      else {
        data→numbers→set_at($index, 0);
      }
    }
  }
  code→misc_code;
  if data→endgame→equals(true) then {
    if data→numbers→min = 1 then {
      data→text→set_text("Draw!");
      data→text→set_color(colors→light_gray);
      if data→scoreadded→equals(false) then {
        records→scores_index→at(2)→x→add(1);
        data→scoreadded := true;
      }
    }
    else {
      if data→currentplayer = 1 then {
        data→text→set_text("Player 2 won");
        data→text→set_color(colors→light_gray);
        if data→scoreadded→equals(false) then {
          records→scores_index→at(1)→x→add(1);
          data→scoreadded := true;
        }
      }
      else {
        data→text→set_text("PLayer 1 win!!");
        data→text→set_color(colors→light_gray);
        if data→scoreadded→equals(false) then {
          records→scores_index→at(0)→x→add(1);
          data→scoreadded := true;
        }
      }
    }
  }
  else {
    if data→currentplayer = 1 then {
      data→text→set_color(colors→accent);
    }
    else {
      data→text→set_color(colors→accent);
      if data→numbers→min = 1 then {
        data→endgame := true;
      }
      code→disp_scores;
      data→board→update_on_wall;
    }
  }
  meta private;
}
var boxes : Sprite_Set {
}
var currentplayer : Number {
}
var endgame : Boolean {
}
var numbers : Number_Collection {
}
var resetscores : Sprite {
}
var scoreadded : Boolean {
}
var scoretext : Sprite_Set {
}
var text : Sprite {
}
table scores {
  type = "Index";
  keys {
    number: Number
  }
  fields {
    x: Number_field
  }
}

event tap_sprite\u003A_resetscores(sprite: Sprite, x: Number, y: Number) {
  $b := wall→ask_boolean("Do you want to reset scores?", "");
  if $b→equals(true) then {
    records→scores_index→at(0)→x→set(0);
    records→scores_index→at(1)→x→set(0);
    records→scores_index→at(2)→x→set(0);
    wall→clear;
    data→board→post_to_wall;
    skip;
  }
  else {
    wall→clear;
    data→board→post_to_wall;
  }
  skip;
  meta private;
}

action check(num1: Number, num2: Number, num3: Number) {
  if $num1 + $num2 + $num3 = 3 then {
    data→endgame := true;
  }
  else {
    if $num1 + $num3 + $num2 = 15 then {
      data→endgame := true;
    }
    else {
      data→endgame := false;
    }
  }
  meta private;
}

action disp_scores() {
  data→scoretext→at(0)→set_text(records→scores_index→at(0)→x→get→to_string);
  data→scoretext→at(1)→set_text(records→scores_index→at(1)→x→get→to_string);
  data→scoretext→at(2)→set_text(records→scores_index→at(2)→x→get→to_string);
  meta private;
}

event tap_sprite_in_boxes(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if data→endgame→equals(false) then {
    if data→currentplayer = 1 then {
      if $sprite→color→equals(colors→light_gray) then {
        $sprite→set_color(colors→accent);
        data→currentplayer := 2;
      }
    }
    else {
      if $sprite→color→equals(colors→light_gray) then {
        $sprite→set_color(colors→accent);
        data→currentplayer := 1;
      }
    }
  }
  data→board→update_on_wall;
  meta private;
}

action misc_code() {
  if data→endgame→equals(false) then {
    code→check(data→numbers→at(0), data→numbers→at(1), data→numbers→at(2));
  }
  if data→endgame→equals(false) then {
    code→check(data→numbers→at(3), data→numbers→at(4), data→numbers→at(5));
  }
  if data→endgame→equals(false) then {
    code→check(data→numbers→at(6), data→numbers→at(7), data→numbers→at(8));
  }
  if data→endgame→equals(false) then {
    code→check(data→numbers→at(0), data→numbers→at(3), data→numbers→at(6));
  }
  if data→endgame→equals(false) then {
    code→check(data→numbers→at(1), data→numbers→at(4), data→numbers→at(7));
  }
  if data→endgame→equals(false) then {
    code→check(data→numbers→at(2), data→numbers→at(5), data→numbers→at(8));
  }
  if data→endgame→equals(false) then {
    code→check(data→numbers→at(0), data→numbers→at(4), data→numbers→at(8));
  }
  if data→endgame→equals(false) then {
    code→check(data→numbers→at(6), data→numbers→at(4), data→numbers→at(2));
  }
  meta private;
}

