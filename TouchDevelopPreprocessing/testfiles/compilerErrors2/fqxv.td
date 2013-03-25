meta version "v2.2,nothing";
meta name "PictureLogic";
meta icon "emergency";
meta color "#ff004b49";
//
meta platform "current";

action main() {
  data→chosenBtn := 0;
  records→Rows_table→clear;
  records→Levels_table→clear;
  records→Level_index→clear;
  if records→Levels_table→count = 0 then {
    code→addLevels;
  }
  code→createPageSizes;
}
var board : Board {
}
table Cell {
  type = "Index";
  keys {
    x: Number
    y: Number
  }
  fields {
    mark: Boolean_field
    icon: Sprite_field
    iconState: Number_field
  }
}
var cellSize : Number {
}

action setRow(row: Number, cellDef: String) {
  $col := 0;
  foreach s in $cellDef where true do {
    $col := $col + 1;
    $c := records→Cell_index→at($col, $row);
    if $s→equals("1") then {
      $c→mark→set(true);
    }
    else {
      $c→mark→set(false);
    }
    $icon1 := data→board→create_picture(records→CellPics_index→at(data→stateBlank)→pic→get→clone);
    $icon1→set_pos(($col - 0.5) * data→cellSize, ($row - 0.5) * data→cellSize + 240);
    $c→icon→set($icon1);
    $c→iconState→set(data→stateBlank);
    data→cellIcons→add($icon1);
    $iconPos := records→icon_index→at(data→cellIcons→count);
    $iconPos→x→set($col);
    $iconPos→y→set($row);
  }
  ... ;
  meta private;
}
var cellIcons : Sprite_Set {
}

event tap_sprite_in_cellIcons(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  $idx := records→icon_index→at($index_in_set + 1);
  $c := records→Cell_index→at($idx→x→get, $idx→y→get);
  if data→chosenBtn = 1 then {
    if $c→mark→get then {
      $c→iconState→set(data→stateOk);
      $c→icon→get→set_picture(records→CellPics_index→at(data→stateOk)→pic→get→clone);
      code→checkWin;
    }
    else {
      $c→iconState→set(data→stateError);
      $picError := records→CellPics_index→at(data→stateError)→pic→get→clone;
      $picError→resize(data→cellSize, - 1);
      $c→icon→get→set_picture($picError);
      code→checkLoose;
    }
  }
  else {
    if data→chosenBtn = 2 then {
      if $c→iconState→get ≠ data→stateError then {
        $c→iconState→set(data→stateNot);
        $c→icon→get→set_picture(records→CellPics_index→at(data→stateNot)→pic→get→clone);
      }
    }
  }
  data→board→update_on_wall;
  meta private;
}
table icon {
  type = "Index";
  keys {
    iconIdx: Number
  }
  fields {
    x: Number_field
    y: Number_field
  }
}
var output : Sprite {
}

action initPictures() {
  $ps := data→cellSize - 1;
  // Initialize constants
  data→stateBlank := 1;
  data→stateNot := 2;
  data→stateOk := 3;
  data→stateError = 4;
  // Initialize the blank picture
  records→CellPics_index→at(data→stateBlank)→pic→set(media→create_picture($ps, $ps));
  // Initialize the picture for correct positive guesses
  $picOk := media→create_picture(100, 100);
  $picOk→fill_ellipse(5, 5, 90, 90, 0, colors→accent);
  $pic := media→icon("CheckAlt");
  $pic→colorize(colors→transparent, colors→black, 0.65);
  $pic→resize(80, 80);
  $picOk→blend($pic, 10, 10, 0, 1);
  $picOk→resize($ps, $ps);
  records→CellPics_index→at(data→stateOk)→pic→set($picOk);
  // Initialize the pic for negative guesses
  $picNot := media→create_picture(100, 100);
  $picNot→fill_ellipse(40, 40, 20, 20, 0, colors→subtle);
  $picNot→resize($ps, $ps);
  records→CellPics_index→at(data→stateNot)→pic→set($picNot);
  // Initialize the picture for wrong guesses
  $picError := media→create_picture(100, 100);
  $picError→fill_rect(30, 10, 85, 30, 45, colors→red);
  $picError→fill_rect(90, 30, 85, 30, 135, colors→red);
  $picError→resize(70, - 1);
  records→CellPics_index→at(data→stateError)→pic→set($picError);
  meta private;
}
var stateError : Number {
}
var stateBlank : Number {
}
var stateOk : Number {
}
var stateNot : Number {
}

action createMenuItem(size: String) {
  $tb := wall→create_text_box($size, 50);
  $tb→set_border(colors→white);
  $tb→post_to_wall;
  meta private;
}
var pageSizes : Page {
}
var page5x5 : Page {
}

action createPageSizes() {
  data→pageSizes := wall→current_page;
  wall→set_reversed(true);
  code→createMenuItem("5x5");
  code→createMenuItem("10x10");
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  if wall→current_page→equals(data→pageSizes) then {
    if $item→text→equals("5x5") then {
      code→createPage5x5;
    }
    else {
      if $item→text→equals("10x10") then {
        code→createPage10x10;
      }
    }
  }
  else {
    if wall→current_page→equals(data→page5x5) then {
      code→initLevelUI($item→text, 5);
    }
    else {
      if wall→current_page→equals(data→page10x10) then {
        code→initLevelUI($item→text, 10);
      }
    }
  }
  meta private;
}

action createPage5x5() {
  data→page5x5 := wall→push_new_page;
  wall→set_reversed(true);
  foreach l in records→Levels_table where $l→Size→get = 5 do {
    code→createMenuItem($l→name→get);
  }
  meta private;
}
var page10x10 : Page {
}

action createPage10x10() {
  data→page10x10 := wall→push_new_page;
  wall→set_reversed(true);
  foreach l in records→Levels_table where $l→Size→get = 10 do {
    code→createMenuItem($l→name→get);
  }
  meta private;
}

action initLevelUI(name: String, size: Number) {
  wall→push_new_page;
  data→board := media→create_full_board;
  data→errors := 0;
  // Set board background
  $pic := media→create_picture(480, 800);
  // Draw background border
  $pic→draw_line(0, 60, 0, 600, colors→white, 1);
  $pic→draw_line(0, 60, 480, 60, colors→white, 1);
  $pic→draw_line(480, 60, 480, 600, colors→white, 1);
  $pic→draw_line(0, 600, 480, 600, colors→white, 1);
  // Draw background grid
  data→cellSize := 360 / $size;
  for 0 ≤ i < $size do {
    $pos := ($i + 1) * data→cellSize;
    $pic→draw_line($pos, 60, $pos, 600, colors→gray, 1);
    $pic→draw_line(0, ($i * data→cellSize) + 240, 480, ($i * data→cellSize) + 240, colors→gray, 1);
  }
  data→board→set_background_picture($pic);
  // Initialize level variables
  records→Cell_index→clear;
  records→icon_index→clear;
  data→cellIcons := data→board→create_sprite_set;
  code→initPictures;
  // Create level UI
  $r := 0;
  $l := records→Level_index→at($name)→level→get;
  foreach row in records→Rows_table where $row→Level→get→equals($l) do {
    $r := $r + 1;
    code→setRow($r, $row→rowDef→get);
    code→hintRow($row→rowDef→get, ($r - 0.5) * data→cellSize + 223);
  }
  for 0 ≤ i1 < $size do {
    code→hintCol($i1, $l);
  }
  // Initialize buttons
  data→btnList := data→board→create_sprite_set;
  $pic1 := media→create_picture(100, 100);
  $pic1→draw_rect(0, 0, 100, 100, 0, colors→white, 3);
  $pic2 := media→icon("Check");
  $pic2→resize(100, - 1);
  $pic1→blend($pic2, 0, 0, 0, 1);
  $sprite := data→board→create_picture($pic1);
  $sprite→set_pos(50, 700);
  data→btnList→add($sprite);
  $pic1 := media→create_picture(100, 100);
  $pic1→draw_rect(0, 0, 100, 100, 0, colors→white, 3);
  $pic2 := media→icon("Multiply");
  $pic2→resize(100, - 1);
  $pic1→blend($pic2, 0, 0, 0, 1);
  $sprite := data→board→create_picture($pic1);
  $sprite→set_pos(200, 700);
  data→btnList→add($sprite);
  // Create error symbols
  $sprite := data→board→create_picture(records→CellPics_index→at(data→stateError)→pic→get→clone);
  $sprite→set_pos(310, 700);
  $sprite→set_opacity(0.1);
  data→btnList→add($sprite);
  $sprite := data→board→create_picture(records→CellPics_index→at(data→stateError)→pic→get→clone);
  $sprite→set_pos(380, 700);
  $sprite→set_opacity(0.1);
  data→btnList→add($sprite);
  $sprite := data→board→create_picture(records→CellPics_index→at(data→stateError)→pic→get→clone);
  $sprite→set_pos(450, 700);
  $sprite→set_opacity(0.1);
  data→btnList→add($sprite);
  data→output := data→board→create_text(480, 20, 40, "");
  data→output→set_pos(240, 20);
  data→board→post_to_wall;
  meta private;
}
table CellPics {
  type = "Index";
  keys {
    idx: Number
  }
  fields {
    pic: Picture_field
  }
}
table Rows {
  type = "Table";
  fields {
    Level: Levels_field
    rowDef: String_field
  }
}
table Level {
  type = "Index";
  keys {
    name: String
  }
  fields {
    level: Levels_field
  }
}
table Levels {
  type = "Table";
  fields {
    name: String_field
    Size: Number_field
  }
}

action addLevels() {
  $l := code→addLevel("5x5 Level 1", 5);
  code→addLevelRow($l, "00100");
  code→addLevelRow($l, "01110");
  code→addLevelRow($l, "11111");
  code→addLevelRow($l, "01110");
  code→addLevelRow($l, "00100");
  $l := code→addLevel("5x5 Level 2", 5);
  code→addLevelRow($l, "10101");
  code→addLevelRow($l, "01110");
  code→addLevelRow($l, "11111");
  code→addLevelRow($l, "01110");
  code→addLevelRow($l, "10101");
  $l := code→addLevel("5x5 Level 3", 5);
  code→addLevelRow($l, "11101");
  code→addLevelRow($l, "01111");
  code→addLevelRow($l, "11111");
  code→addLevelRow($l, "11110");
  code→addLevelRow($l, "10111");
  $l := code→addLevel("5x5 Level 4", 5);
  code→addLevelRow($l, "00100");
  code→addLevelRow($l, "01110");
  code→addLevelRow($l, "11111");
  code→addLevelRow($l, "10101");
  code→addLevelRow($l, "10111");
  $l := code→addLevel("5x5 Level 5", 5);
  code→addLevelRow($l, "11111");
  code→addLevelRow($l, "11011");
  code→addLevelRow($l, "10001");
  code→addLevelRow($l, "11011");
  code→addLevelRow($l, "11111");
  $l := code→addLevel("10x10 Level 1", 10);
  code→addLevelRow($l, "0011000110");
  code→addLevelRow($l, "0111101111");
  code→addLevelRow($l, "0111111111");
  code→addLevelRow($l, "0111111111");
  code→addLevelRow($l, "0111111111");
  code→addLevelRow($l, "0011111110");
  code→addLevelRow($l, "0001111100");
  code→addLevelRow($l, "0000111000");
  code→addLevelRow($l, "0000010000");
  code→addLevelRow($l, "0000000000");
  meta private;
}

action addLevelRow(level: Levels, rowDef: String) {
  $r := records→Rows_table→add_row;
  $r→Level→set($level);
  $r→rowDef→set($rowDef);
  meta private;
}

action addLevel(name: String, size: Number) returns level: Levels {
  $level := records→Levels_table→add_row;
  $level→name→set($name);
  $level→Size→set($size);
  records→Level_index→at($name)→level→set($level);
  meta private;
}
var btnList : Sprite_Set {
}

event tap_sprite_in_btnList(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $index_in_set = 1 then {
    // No clicked
    data→chosenBtn := 2;
    data→btnList→at(0)→set_opacity(1);
    data→btnList→at(1)→set_opacity(0.5);
  }
  else {
    // Ok clicked
    data→chosenBtn := 1;
    data→btnList→at(0)→set_opacity(0.5);
    data→btnList→at(1)→set_opacity(1);
  }
  data→board→update_on_wall;
  meta private;
}

action checkWin() {
  $solved := true;
  foreach c in records→Cell_index where true do {
    if $c→mark→get and $c→iconState→get ≠ data→stateOk then {
      $solved := false;
    }
  }
  if $solved then {
    data→output→set_text("Congratulations");
    time→sleep(2);
    wall→pop_page;
  }
  meta private;
}

action hintRow(rowDef: String, y: Number) {
  $x := 370;
  $hint := 0;
  foreach s in $rowDef where true do {
    if $s→equals("1") then {
      $hint := $hint + 1;
    }
    else {
      if $hint > 0 then {
        $sprite := data→board→create_text(10, 20, 40, $hint→to_string);
        $sprite→set_pos($x, $y);
        $hint := 0;
        $x := $x + 20;
      }
    }
  }
  if $x = 370 or $hint > 0 then {
    $sprite1 := data→board→create_text(10, 20, 40, $hint→to_string);
    $sprite1→set_pos($x, $y);
  }
  meta private;
}

action hintCol(col: Number, level: Levels) {
  $y := 200;
  $hint := 0;
  $colDef := "";
  foreach r in records→Rows_table where $r→Level→get→equals($level) do {
    $s := $r→rowDef→get→at($col);
    $colDef := $s ∥ $colDef;
  }
  foreach s2 in $colDef where true do {
    if $s2→equals("1") then {
      $hint := $hint + 1;
    }
    else {
      if $hint > 0 then {
        $sprite := data→board→create_text(10, 20, 40, $hint→to_string);
        $sprite→set_pos(($col + 0.5) * data→cellSize, $y);
        $hint := 0;
        $y := $y - 30;
      }
    }
  }
  if $y = 200 or $hint > 0 then {
    $sprite1 := data→board→create_text(10, 20, 40, $hint→to_string);
    $sprite1→set_pos(($col + 0.5) * data→cellSize, $y);
  }
  meta private;
}
var chosenBtn : Number {
}
var errors : Number {
}

action checkLoose() {
  data→errors := data→errors + 1;
  data→btnList→at(data→errors + 1)→set_opacity(1);
  if data→errors ≥ 3 then {
    data→output→set_text("Game over");
    data→board→update_on_wall;
    time→sleep(2);
    wall→pop_page;
  }
  meta private;
}

