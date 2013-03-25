meta version "v2.2,nothing";
meta name "Tic-Tac-Toe";
meta icon "ninecolumn";
meta color "#ff008000";
// A two player game of tic-tac-toe.
//
// v1.1
// • storing scores
//
// v1.1a
// • added icon to score reset button
meta platform "current";

action main() {
  data→endGame := false;
  data→currentPlayer := 1;
  data→board := media→create_full_board;
  data→boxes := data→board→create_sprite_set;
  data→scoreText := data→board→create_sprite_set;
  data→scoreAdded := true;
  data→resetScores := data→board→create_picture(media→large_icon("trash"));
  data→resetScores→set_pos(392, 718);
  data→numbers := collections→create_number_collection;
  for 0 ≤ i < 9 do {
    data→numbers→add(0);
  }
  $num := 0;
  for 0 ≤ i2 < 3 do {
    for 0 ≤ i1 < 3 do {
      data→boxes→add(data→board→create_rectangle(140, 140));
      data→boxes→at($num)→set_pos(85 + 155 * $i2, 165 + 155 * $i1);
      $num := $num + 1;
    }
  }
  for 0 ≤ i3 < 3 do {
    data→scoreText→add(data→board→create_text(100, 20, 60, ""));
    data→scoreText→at($i3)→set_pos(50 + 80 * $i3, 685);
  }
  data→scoreText→at(0)→set_color(colors→accent);
  data→scoreText→at(1)→set_color(colors→chrome);
  data→text := data→board→create_text(0, 0, 60, "");
  data→text→set_pos(15, 560);
  data→text→set_text("Current player");
  data→board→post_to_wall;
}
var board : Board {
}
var boxes : Sprite_Set {
}

event tap_sprite_in_boxes(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if data→endGame→equals(false) then {
    if data→currentPlayer = 1 then {
      if $sprite→color→equals(colors→light_gray) then {
        $sprite→set_color(colors→accent);
        data→currentPlayer := 2;
      }
    }
    else {
      if $sprite→color→equals(colors→light_gray) then {
        $sprite→set_color(colors→chrome);
        data→currentPlayer := 1;
      }
    }
  }
  data→board→update_on_wall;
  meta private;
}

event gameloop() {
  foreach sprite in data→boxes where true do {
    $index := data→boxes→index_of($sprite);
    if $sprite→color→equals(colors→accent) then {
      data→numbers→set_at($index, 1);
    }
    else {
      if $sprite→color→equals(colors→chrome) then {
        data→numbers→set_at($index, 5);
      }
      else {
        data→numbers→set_at($index, 0);
      }
    }
  }
  code→spaghetti_code;
  if data→endGame→equals(true) then {
    if data→numbers→min = 1 then {
      data→text→set_text("Draw!");
      data→text→set_color(colors→light_gray);
      if data→scoreAdded→equals(false) then {
        records→scores_index→at(2)→x→add(1);
        data→scoreAdded := true;
      }
    }
    else {
      if data→currentPlayer = 1 then {
        data→text→set_text("Player 2 won!");
        data→text→set_color(colors→light_gray);
        if data→scoreAdded→equals(false) then {
          records→scores_index→at(1)→x→add(1);
          data→scoreAdded := true;
        }
      }
      else {
        data→text→set_text("Player 1 won!");
        data→text→set_color(colors→light_gray);
        if data→scoreAdded→equals(false) then {
          records→scores_index→at(0)→x→add(1);
          data→scoreAdded := true;
        }
      }
    }
  }
  else {
    if data→currentPlayer = 1 then {
      data→text→set_color(colors→accent);
    }
    else {
      data→text→set_color(colors→chrome);
    }
  }
  if data→numbers→min = 1 then {
    data→endGame := true;
  }
  code→display_scores;
  data→board→update_on_wall;
  meta private;
}
var numbers : Number_Collection {
}
var text : Sprite {
}

action check(num1: Number, num2: Number, num3: Number) {
  if $num1 + $num2 + $num3 = 3 then {
    data→endGame := true;
  }
  else {
    if $num1 + $num2 + $num3 = 15 then {
      data→endGame := true;
    }
    else {
      data→endGame := false;
    }
  }
  meta private;
}
var endGame : Boolean {
}

action spaghetti_code() {
  if data→endGame→equals(false) then {
    code→check(data→numbers→at(0), data→numbers→at(1), data→numbers→at(2));
  }
  if data→endGame→equals(false) then {
    code→check(data→numbers→at(3), data→numbers→at(4), data→numbers→at(5));
  }
  if data→endGame→equals(false) then {
    code→check(data→numbers→at(6), data→numbers→at(7), data→numbers→at(8));
  }
  if data→endGame→equals(false) then {
    code→check(data→numbers→at(0), data→numbers→at(3), data→numbers→at(6));
  }
  if data→endGame→equals(false) then {
    code→check(data→numbers→at(1), data→numbers→at(4), data→numbers→at(7));
  }
  if data→endGame→equals(false) then {
    code→check(data→numbers→at(2), data→numbers→at(5), data→numbers→at(8));
  }
  if data→endGame→equals(false) then {
    code→check(data→numbers→at(0), data→numbers→at(4), data→numbers→at(8));
  }
  if data→endGame→equals(false) then {
    code→check(data→numbers→at(6), data→numbers→at(4), data→numbers→at(2));
  }
  meta private;
}
var currentPlayer : Number {
}
var scoreText : Sprite_Set {
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

action display_scores() {
  data→scoreText→at(0)→set_text(records→scores_index→at(0)→x→get→to_string);
  data→scoreText→at(1)→set_text(records→scores_index→at(1)→x→get→to_string);
  data→scoreText→at(2)→set_text(records→scores_index→at(2)→x→get→to_string);
  meta private;
}
var scoreAdded : Boolean {
}
var resetScores : Sprite {
}

event tap_sprite\u003A_resetScores(sprite: Sprite, x: Number, y: Number) {
  $b := wall→ask_boolean("Do you want to reset scores?", "");
  if $b→equals(true) then {
    records→scores_index→at(0)→x→set(0);
    records→scores_index→at(1)→x→set(0);
    records→scores_index→at(2)→x→set(0);
    wall→clear;
    data→board→post_to_wall;
  }
  else {
    wall→clear;
    data→board→post_to_wall;
  }
  meta private;
}

