meta version "v2.2,nothing";
meta name "CountersHome";
meta icon "HomeAlt";
meta color "#ff007FFF";
// Keep history all your Home counters, such as electricity, water, Gas, etc...
meta platform "accelerometer,calendar,camera,location,maps,search,translation";

action main() {
  data→senseSpinner := false;
  data→x\_done := false;
  wall→set_title("Counters");
  if records→Counters_table→count = 0 then {
    wall→prompt("No counter Defined !!! Please create a counter.");
    code→newCounter;
  }
  data→boardSpinner := invalid→board;
  data→spinnerWasDragged := false;
  code→CounterSelect;
}
table Counters {
  type = "Table";
  fields {
    Name: String_field
    digits: Number_field
    decimals: Number_field
    highlightedPos: Number_field
    bCommentEnabled: Boolean_field
    xStatInterval: Number_field
  }
}

action newCounter() {
  wall→push_new_page;
  wall→set_reversed(true);
  $s := wall→ask_string("Counteur Name?");
  if $s→is_empty then {
    wall→prompt("Counter name cannot be empty! Création aborted.");
  }
  else {
    "The total of digits + decimals shall not excess 7. If no decimals are used, the number of digits can be up to 8." →post_to_wall;
    $digits := wall→ask_number("How many digits?");
    $decimals := wall→ask_number("How many decimals?");
    $digits := math→round($digits);
    $decimals := math→round($decimals);
    wall→clear;
    if $digits > 0 and $decimals ≥ 0 then {
      if(($decimals > 0) and ($digits + $decimals ≤ 7)) or (($decimals = 0) and $digits ≤ 8) then {
        $c := records→Counters_table→add_row;
        data→selectedCounter := $c;
        code→setCommentProp;
        $c→Name→set($s);
        $c→digits→set($digits);
        $c→decimals→set($decimals);
        $c→post_to_wall;
        records→Counters_table→count→to_string→post_to_wall;
        wall→prompt("Counter Created");
        wall→pop_page;
      }
      else {
        wall→prompt("Digits + decimals quantity condition bot fulfilled. Creation aborted.");
      }
    }
    else {
      wall→prompt("Invalid digit or decimal value! Creation aborted.");
    }
  }
  meta private;
}

action FillCounterStrCollection() {
  $strings := collections→create_string_collection;
  foreach c in records→Counters_table where not $c→Name→get→is_empty do {
    $strings→add($c→Name→get);
  }
  data→colCounterNames := $strings;
  meta private;
}

action SelectACounter() {
  $x := wall→pick_string("Counters", "Select a counter", data→colCounterNames);
  foreach c in records→Counters_table where $c→Name→get→equals(data→colCounterNames→at($x)) do {
    data→selectedCounter := $c;
    code;
  }
  meta private;
}
var selectedCounter : Counters {
}
var colCounterNames : String_Collection {
}

action storeCounterValue(val: Number, dateTime: DateTime, comment: String) {
  if not data→selectedCounter→is_invalid then {
    $c := records→counterValIndex_index→at(data→selectedCounter→Name→get, $dateTime);
    $c→val→set($val);
    $c→cName→set(data→selectedCounter→Name→get);
    $c→dt\_val→set($dateTime);
    $c→comment→set($comment);
    $c→year→set($dateTime→year);
    $c→month→set($dateTime→month);
    $c→day→set($dateTime→day);
    $c→hour→set($dateTime→hour);
    $c→minute→set($dateTime→minute);
    wall→push_new_page;
    wall→prompt("Value saved");
    wall→pop_page;
  }
  meta private;
}

action CounterReadEntry() {
  wall→push_new_page;
  wall→set_title(data→selectedCounter→Name→get);
  wall→set_subtitle("Mark Readings");
  wall→add_button("Save", "Save");
  wall→add_button("Feature.settings", "Sense");
  wall→add_button("back", "back");
  wall→add_button("Next", "Log");
  code→CreateSpinnerBoard;
  meta private;
}
var boardSpinner : Board {
}
var rectSpritesSet : Sprite_Set {
}
var SPRITE\_H : Number {
}
var SPRITE\_W : Number {
}

event gameloop() {
  time→log("Gameloop entered");
  if not data→boardSpinner→is_invalid then {
    if( not data→boardSpinner→touched) and data→spinnerWasDragged then {
      time→log("Gameloop: dragged");
      code→FixSpinnerPosition;
    }
    else {
      time→log("Gameloop: senseSpinner");
      code→senseSpinnerProcess;
    }
  }
  meta private;
}
var sprite\_offset\_y : Number {
}

event drag_sprite_in_rectSpritesSet(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  code→DragSpritesAction($delta_y, true);
  meta private;
}
var textSpritesSet : Sprite_Set {
}

event drag_sprite_in_textSpritesSet(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  code→DragSpritesAction($delta_y, true);
  meta private;
}

action DragSpritesAction(delta_y: Number, dragged: Boolean) {
  data→spinnerWasDragged := $dragged;
  time→log("Drag sprite action, dragged=" ∥ $dragged→to_string);
  $b := data→rectSpritesSet→at(0)→y < data→boardSpinner→height / 2 or $delta_y < 0;
  if((data→rectSpritesSet→at(9)→y > data→boardSpinner→height / 2) or $delta_y > 0) and $b then {
    foreach sprite in data→rectSpritesSet where true do {
      $sprite→set_pos($sprite→x, $sprite→y + $delta_y);
    }
    foreach sprite1 in data→textSpritesSet where true do {
      $sprite1→set_pos($sprite1→x, $sprite1→y + $delta_y);
    }
  }
  data→boardSpinner→evolve;
  data→boardSpinner→update_on_wall;
  time→log("Drag spinner sprites end");
  meta private;
}
var counterVal : Number {
}
var digitsSpriteSet : Sprite_Set {
}
var highlighted : Number {
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→icon→equals("add") then {
    code→newCounter;
    code→FillCounterStrCollection;
    wall→pop_page;
  }
  else {
    if $item→icon→equals("question mark") then {
      code→aboutpage;
    }
  }
  $x1 := data→selectedCounter→decimals→get + data→selectedCounter→digits→get;
  if data→selectedCounter→decimals→get > 0 then {
    $x1 := $x1 + 1;
  }
  if $item→icon→equals("add") then {
    code→CounterReadEntry;
    if $item→icon→equals("Save") then {
      $dt := time→now;
      $str := "";
      for 0 ≤ i < $x1 do {
        $str := $str→concat(data→digitsSpriteSet→at($i)→text);
      }
      wall→push_new_page;
      if wall→ask_boolean("Do you want to enter a custom date?", "Date: " ∥ time→now→to_string) then {
        $dt1 := wall→pick_date(time→now→date→to_string, "pick a date");
        $dt := $dt1;
      }
      $s1 := "";
      if data→selectedCounter→bCommentEnabled→get then {
        $s1 := wall→ask_string("Enter a comment (optional)");
      }
      wall→pop_page;
      skip;
      code→storeCounterValue($str→to_number, $dt, $s1);
      code→CounterPage;
    }
    else {
      if $item→icon→equals("Delete") then {
        wall→push_new_page;
        code→clearSomeEntries;
        wall→pop_page;
      }
      else {
        if $item→icon→equals("Feature.settings") then {
          data→senseSpinner := not data→senseSpinner;
          wall→push_new_page;
          if data→senseSpinner then {
            wall→prompt("Accelerometer spinner control activated");
          }
          else {
            wall→prompt("Accelerometer spinner control deactivated");
          }
          wall→pop_page;
        }
        else {
          if $item→icon→equals("Feature.search") then {
            code→PushCounterStatsPage;
          }
        }
      }
    }
  }
  else {
    if $item→icon→equals("Back") then {
      wall→pop_page;
    }
    else {
      if $item→icon→equals("Save") then {
        $dt2 := time→now;
        $str1 := "";
        for 0 ≤ i1 < $x1 do {
          $str1 := $str1→concat(data→digitsSpriteSet→at($i1)→text);
        }
        wall→push_new_page;
        if wall→ask_boolean("Do you want to enter a custom date?", "Date: " ∥ time→now→to_string) then {
          $dt3 := wall→pick_date(time→now→date→to_string, "pick a date");
          $dt2 := $dt3;
        }
        $s := "";
        if data→selectedCounter→bCommentEnabled→get then {
          $s := wall→ask_string("Enter a comment (optional)");
        }
        wall→pop_page;
        skip;
        code→storeCounterValue($str1→to_number, $dt2, $s);
        code→CounterPage;
      }
      else {
        if $item→icon→equals("Delete") then {
          wall→push_new_page;
          code→clearSomeEntries;
          wall→pop_page;
        }
        else {
          if $item→icon→equals("Feature.settings") then {
            data→senseSpinner := not data→senseSpinner;
            wall→push_new_page;
            if data→senseSpinner then {
              wall→prompt("Accelerometer spinner control activated");
            }
            else {
              wall→prompt("Accelerometer spinner control deactivated");
            }
            wall→pop_page;
          }
          else {
            if $item→icon→equals("Feature.search") then {
              code→PushCounterStatsPage;
            }
          }
        }
      }
    }
    skip;
  }
  meta private;
}
var sprite\_gap\_x : Number {
}

action FixSpinnerPosition() {
  $y := 0;
  // Highlights the center spinner sprite and determine the offset to align the sprite with the middle of the Biard on Y axis
  time→log("FixSpinnerPos");
  foreach sprite in data→rectSpritesSet where true do {
    if $sprite→y < data→spinner\_center\_bottom and $sprite→y > data→spinner\_center\_top then {
      $sprite→set_color($sprite→color→make_transparent(0.5));
      $y := $sprite→y - data→boardSpinner→height / 2;
    }
    else {
      $sprite→set_color($sprite→color→make_transparent(0.1));
    }
    $sprite→set_x(data→spinner\_x\_pos);
  }
  // Positions the sprites according to the offset determined above
  foreach sprite1 in data→rectSpritesSet where true do {
    $sprite1→set_y($sprite1→y - $y);
  }
  foreach sprite2 in data→textSpritesSet where true do {
    $sprite2→set_x(data→spinner\_x\_pos);
    $sprite2→set_y($sprite2→y - $y);
  }
  if data→digitsSpriteSet→count ≥ data→highlighted then {
    data→digitsSpriteSet→at(data→highlighted)→set_text(code→GetSpinnerValue→to_string);
  }
  data→spinnerWasDragged := false;
  data→boardSpinner→update_on_wall;
  time→log("FixSpinnerPos end");
  meta private;
}

action updateDigitSprites() {
  $x1 := data→selectedCounter→decimals→get + data→selectedCounter→digits→get;
  if data→selectedCounter→decimals→get > 0 then {
    $x1 := $x1 + 1;
  }
  for 0 ≤ i < $x1 do {
    if $i = data→highlighted then {
      data→digitsSpriteSet→at($i)→hide;
    }
    else {
      data→digitsSpriteSet→at($i)→show;
    }
  }
  data→spinner\_x\_pos := data→digitsSpriteSet→at(data→highlighted)→x;
  meta private;
}
var sprite\_gap\_y : Number {
}
var spinner\_center\_bottom : Number {
}
var spinner\_center\_top : Number {
}
var spinner\_x\_pos : Number {
}

action SetSpinnerValue(val: Number) {
  time→log("Set spinner position");
  $tranparency := 0;
  for 0 ≤ i < 10 do {
    $y := data→boardSpinner→height / 2 - (($val - $i) * (data→SPRITE\_H + data→sprite\_gap\_y));
    data→rectSpritesSet→at($i)→set_y($y);
    data→textSpritesSet→at($i)→set_y($y);
    if $i = $val then {
      $tranparency := 0.5;
    }
    else {
      $tranparency := 0.1;
    }
    data→rectSpritesSet→at($i)→set_color(data→rectSpritesSet→at($i)→color→make_transparent($tranparency));
  }
  meta private;
}

action GetSpinnerValue() returns val: Number {
  $closerPos := data→boardSpinner→height / 2;
  $endLoop := false;
  $i := 0;
  $val := 9;
  while $endLoop→equals(false) do {
    $y := math→abs(data→rectSpritesSet→at($i)→y - data→boardSpinner→height / 2);
    if $y < $closerPos then {
      $val := $i;
      $closerPos := $y;
    }
    $i := $i + 1;
    if $i > 9 then {
      $endLoop := true;
    }
  }
  meta private;
}

action GetCounterVal(selCounter: Counters, refDate: DateTime) returns val: Number, dateVal: DateTime, valid: Boolean {
  $tdiff := math→\u221E\u208A;
  $val := 0;
  $dateVal := time→now;
  $valid := false;
  foreach c in records→counterValIndex_index where $c→counterName→equals($selCounter→Name→get) do {
    if($refDate→subtract($c→dt\_val→get) < $tdiff) and not $c→dt\_val→get→greater_or_equal($refDate) then {
      $val := $c→val→get;
      $dateVal := $c→dt\_val→get;
      $valid := true;
      $tdiff := $refDate→subtract($dateVal);
    }
  }
  meta private;
}
var boardHistory : Board {
}

action CreateHistoryBoard() {
  data→boardHistory := media→create_board(80);
  data→boardHistory→set_debug_mode(false);
  $sprite := data→boardHistory→create_text(1, 25, 20, "Value");
  $sprite3 := data→boardHistory→create_text(1, 25, 20, "Date");
  $sprite→set_x(0);
  $sprite→set_y(10);
  $sprite3→set_x(240);
  $sprite3→set_y(10);
  for 0 ≤ i < 4 do {
    $sprite1 := data→boardHistory→create_text(240, 25, 20, "");
    $sprite1→set_x(120);
    $sprite1→set_y($i * 30 + 40);
    $sprite2 := data→boardHistory→create_text(240, 25, 20, "");
    $sprite2→set_y($sprite1→y);
    $sprite2→set_x(360);
  }
  data→boardHistory→post_to_wall;
  meta private;
}

action CreateSpinnerBoard() {
  time→log("CreateSpinnerBoard");
  data→boardSpinner := media→create_board(480);
  data→boardSpinner→set_debug_mode(false);
  data→SPRITE\_H := 60;
  data→SPRITE\_W := 50;
  data→sprite\_gap\_x := 0.1 * data→SPRITE\_H;
  data→sprite\_gap\_y := 0.1 * data→SPRITE\_H;
  data→sprite\_offset\_y := 0;
  data→highlighted := data→selectedCounter→highlightedPos→get;
  data→spinner\_center\_bottom := data→boardSpinner→height / 2 + data→SPRITE\_H / 2 + data→sprite\_gap\_y / 2;
  data→spinner\_center\_top := data→boardSpinner→height / 2 - data→SPRITE\_H / 2 - data→sprite\_gap\_y / 2;
  data→boardSpinner→post_to_wall;
  data→rectSpritesSet := data→boardSpinner→create_sprite_set;
  data→textSpritesSet := data→boardSpinner→create_sprite_set;
  data→digitsSpriteSet := data→boardSpinner→create_sprite_set;
  for 0 ≤ i2 < 10 do {
    $aRect := data→boardSpinner→create_rectangle(data→SPRITE\_W, data→SPRITE\_H);
    // Sprite height larger than test height to avoir position bug
    $aText := data→boardSpinner→create_text(data→SPRITE\_W, data→SPRITE\_H * 1.5, data→SPRITE\_H, $i2→to_string);
    $aRect→set_x(data→boardSpinner→width / 2);
    $y := $i2 * (data→SPRITE\_H + data→sprite\_gap\_y) + data→SPRITE\_H / 2;
    $aRect→set_y($y);
    $aText→set_x(data→boardSpinner→width / 2);
    $aText→set_y($y + data→sprite\_offset\_y);
    $aText→set_color(colors→foreground);
    data→rectSpritesSet→add($aRect);
    data→textSpritesSet→add($aText);
  }
  if not data→selectedCounter→is_invalid then {
    // Adds the digits sprites
    $x, $dt, $b := code→GetCounterVal(data→selectedCounter, time→now);
    $s := $x→to_string;
    $dotPos := $s→index_of(".", 0);
    // Adds zeroes for the missing digits
    if $dotPos = - 1 then {
      $dotPos := $s→count;
      $s := $s→concat(".");
      for 0 ≤ i < data→selectedCounter→decimals→get do {
        $s := $s→concat("0");
      }
    }
    // Adds zeroes for the missing digits
    for 0 ≤ i3 < data→selectedCounter→digits→get - $dotPos do {
      $s := "0" →concat($s);
    }
    $x1 := data→selectedCounter→decimals→get + data→selectedCounter→digits→get;
    if data→selectedCounter→decimals→get > 0 then {
      $x1 := $x1 + 1;
    }
    for 0 ≤ i1 < $x1 do {
      // Sprite height larger than test height to avoid position bug
      $digitVal := "";
      if $i1 < $s→count then {
        $digitVal := $s→at($i1);
      }
      $sprite := data→boardSpinner→create_text(data→SPRITE\_W, data→SPRITE\_H * 1.5, data→SPRITE\_H, $digitVal);
      data→digitsSpriteSet→add($sprite);
      $sprite→set_x((data→SPRITE\_W + data→sprite\_gap\_x) * $i1 + data→SPRITE\_W / 2);
      $sprite→set_y(data→boardSpinner→height / 2);
      if $i1 = data→highlighted then {
        $sprite→hide;
      }
    }
    if data→senseSpinner→is_invalid then {
      data→senseSpinner := false;
    }
    code→SetSpinnerValue($s→at(data→highlighted)→to_number);
    code→updateDigitSprites;
    code→DragSpritesAction(0, true);
    code→FixSpinnerPosition;
  }
  meta private;
}

action CounterPage() {
  wall→push_new_page;
  wall→set_title(data→selectedCounter→Name→get);
  wall→set_subtitle("History List Counters");
  wall→add_button("add", "add");
  wall→add_button("Delete", "Delete");
  foreach c in records→counterValIndex_index where $c→counterName→equals(data→selectedCounter→Name→get) do {
    $s\_dt := $c→dt\_val→get→to_string;
    $s\_val := $c→val→get→to_string;
    $s1 := $c→comment→get;
    $s := $s\_dt ∥ "  |  " ∥ $s\_val ∥ " | " ∥ $s1;
    $s→post_to_wall;
  }
  meta private;
}
var nextPageSprite : Sprite {
}

event tap_sprite\u003A_nextPageSprite(sprite: Sprite, x: Number, y: Number) {
  code→CounterDetailChangePage(1);
  meta private;
}
var prevPageSprite : Sprite {
}

event tap_sprite\u003A_prevPageSprite(sprite: Sprite, x: Number, y: Number) {
  code→CounterDetailChangePage( - 1);
  meta private;
}

action CreateMainMenuBoard() {
  $tileSize := 100;
  $tileGap := 10;
  data→mainMenuBoard := media→create_board(640);
  data→mainMenuBoard→set_debug_mode(false);
  $strings := collections→create_string_collection;
  $iconsNames := collections→create_string_collection;
  data→mainMenuSpriteSet := data→mainMenuBoard→create_sprite_set;
  $strings→add("Select \nCounter");
  $strings→add("Create\nCounter");
  $strings→add("Delete\nCounter");
  $strings→add("Send \nData");
  $strings→add("Edit\nCounters");
  $strings→add("Settings");
  $iconsNames→add("add");
  $iconsNames→add("Add");
  $iconsNames→add("Delete");
  $iconsNames→add("Email");
  $iconsNames→add("Edit");
  $iconsNames→add("Settings");
  for 0 ≤ i < $strings→count do {
    $pic := media→create_picture($tileSize, $tileSize);
    $sprite := data→mainMenuBoard→create_picture($pic);
    skip;
    $sprite→set_x(($tileSize + $tileGap) * math→mod($i, 2) + ($tileSize / 2) + $tileGap);
    $sprite→set_y(($tileSize + $tileGap) * math→floor($i / 2) + ($tileSize / 2) + $tileGap);
    $pic→fill_rect(0, 0, $tileSize, $tileSize, 0, colors→accent);
    $pic→draw_text($tileGap, 25, $strings→at($i), 30, 0, colors→foreground);
    $sprite→set_color(colors→blue);
    $sprite→set_picture($pic);
    data→mainMenuSpriteSet→add($sprite);
  }
  data→mainMenuBoard→post_to_wall;
  meta private;
}
var mainMenuSpriteSet : Sprite_Set {
}

event tap_sprite_in_mainMenuSpriteSet(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  $sprite→set_opacity(0.5);
  data→mainMenuBoard→update_on_wall;
  time→sleep(0.3);
  $sprite→set_opacity(1);
  data→mainMenuBoard→update_on_wall;
  if $sprite→is_invalid then {
    "Invalid sprite" →post_to_wall;
  }
  if $index_in_set = 0 then {
    // Select counter
    wall→push_new_page;
    if records→Counters_table→count = 0 then {
      wall→prompt("No counter defined! Create a counter first.");
    }
    else {
      data→counterSelectedAction := 1;
      code→CounterSelect;
    }
  }
  else {
    if $index_in_set = 1 then {
      // Create counter
      wall→push_new_page;
      code→newCounter;
      wall→pop_page;
    }
    else {
      if $index_in_set = 2 then {
        // Delete counter
        wall→push_new_page;
        if records→Counters_table→count = 0 then {
          wall→prompt("No counter defined! Create a counter first.");
          wall→pop_page;
        }
        else {
          data→counterSelectedAction := 2;
          code→CounterSelect;
        }
      }
      else {
        if $index_in_set = 3 then {
          code→SendHistory;
        }
        else {
          if $index_in_set = 4 then {
            // Édit counter
            data→counterSelectedAction := 3;
            wall→push_new_page;
            code→CounterSelect;
          }
          else {
            if $index_in_set = 5 then {
              code→mailSetting;
            }
          }
        }
      }
    }
  }
  meta private;
}
var mainMenuBoard : Board {
}
table counterValIndex {
  type = "Index";
  keys {
    counterName: String
    dt: DateTime
  }
  fields {
    val: Number_field
    comment: String_field
    dt\_val: DateTime_field
    cName: String_field
    day: Number_field
    month: Number_field
    year: Number_field
    hour: Number_field
    minute: Number_field
  }
}

action purgeCounters() {
  $x := 0;
  code→FillCounterStrCollection;
  foreach c in records→Counters_table where $c→Name→get→is_empty do {
    $c→delete_row;
    $x := $x + 1;
  }
  time→log($x→to_string ∥ " counters deleted");
  $x := 0;
  foreach c1 in records→counterValIndex_index where not data→colCounterNames→contains($c1→cName→get) do {
    $c1→clear_fields;
    $x := $x + 1;
  }
  time→log($x→to_string ∥ " counter values purged");
  meta private;
}

action SendHistory() {
  $msg := "";
  foreach c in records→counterValIndex_index where true do {
    $s := $c→cName→get ∥ "; " ∥ $c→dt\_val→get→to_string ∥ "; " ∥ $c→val→get→to_string ∥ ";" ∥ $c→comment→get;
    $s := $s ∥ ";" ∥ $c→year→get→to_string ∥ ";" ∥ $c→month→get→to_string ∥ ";" ∥ $c→day→get→to_string;
    $s := $s ∥ ";" ∥ $c→hour→get→to_string ∥ ";" ∥ $c→minute→get→to_string;
    $msg := $msg ∥ $s ∥ "\n";
  }
  $msg1 := social→create_message($msg);
  $msg1→set_to(data→mailToAddress);
  $msg1→set_title("Home Counters");
  $msg1→share("Email");
  meta private;
}

event drag_sprite_in_mainMenuSpriteSet(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  foreach sprite1 in data→mainMenuSpriteSet where true do {
    $sprite1→set_y($sprite1→y + $delta_y);
  }
  data→mainMenuBoard→update_on_wall;
  meta private;
}
var mailToAddress : String {
}

event tap_sprite_in_digitsSpriteSet(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if not $sprite→text→equals(".") then {
    data→highlighted := $index_in_set;
    code→changeSpinnerDigit;
  }
  meta private;
}
var spinnerWasDragged : Boolean {
}

action CounterDetailChangePage(delta_x: Number) {
  if $delta_x > 0 then {
    code→CounterPage;
  }
  if $delta_x < 0 then {
    wall→pop_page;
  }
  meta private;
}

action clearSomeEntries() {
  $dt\_start := wall→pick_date("Period start date", "pick a date");
  $dt\_end := wall→pick_date("Delete up to date:", "pick a date");
  foreach c in records→counterValIndex_index where $c→cName→get→equals(data→selectedCounter→Name→get) do {
    if $c→dt\_val→get→greater_or_equal($dt\_start) then {
      if $c→dt\_val→get→less_or_equals($dt\_end) then {
        $c→clear_fields;
      }
    }
  }
  meta private;
}
var selectorSpriteSet : Sprite_Set {
}
var SelectorBoard : Board {
}

action CounterSelect() {
  wall→set_subtitle("Select a counter to make readings");
  code→FillCounterStrCollection;
  code→CreateSelectorBoard(data→colCounterNames, "Counters");
  $x := wall→add_button("add", "new");
  skip;
  wall→add_button("question mark", "About");
  meta private;
}
var board : Board {
}
var sprite_set : Sprite_Set {
}

event tap_sprite_in_selectorSpriteSet(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  wall→push_new_page;
  if data→selectorType→to_lower_case→equals("counters") then {
    code→SelectCounter($index_in_set);
  }
  else {
    if data→selectorType→to_lower_case→equals("countersprops") then {
      code→counterPropTapped($index_in_set);
    }
  }
  meta private;
}
var counterSelectedAction : Number {
}

event drag_sprite_in_selectorSpriteSet(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  foreach sprite1 in data→selectorSpriteSet where true do {
    $sprite1→set_y($sprite1→y + $delta_y);
  }
  data→SelectorBoard→update_on_wall;
  meta private;
}

event swipe_sprite_in_textSpritesSet(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  skip;
  meta private;
}
var senseSpinner : Boolean {
}
var x\_done : Boolean {
}

action changeSpinnerDigit() {
  data→selectedCounter→highlightedPos→set(data→highlighted);
  code→updateDigitSprites;
  code→SetSpinnerValue(data→digitsSpriteSet→at(data→highlighted)→text→to_number);
  code→FixSpinnerPosition;
  meta private;
}

action senseSpinnerProcess() {
  if data→senseSpinner then {
    $y\_accel := senses→acceleration_quick→y;
    $x\_accel := senses→acceleration_quick→x;
    time→log($y\_accel→to_string);
    if math→abs($y\_accel) > 0.75 then {
      code→DragSpritesAction(($y\_accel - 0.75) * 100, false);
    }
    else {
      if math→abs($y\_accel) < 0.25 then {
        code→DragSpritesAction(($y\_accel - 0.25) * 100, false);
      }
      else {
        code→FixSpinnerPosition;
      }
    }
    if($x\_accel > 0.5) and not data→x\_done then {
      data→highlighted := data→highlighted + 1;
      if data→highlighted ≥ data→digitsSpriteSet→count then {
        data→highlighted := data→digitsSpriteSet→count - 1;
        phone→vibrate(0.1);
      }
      else {
        if data→digitsSpriteSet→at(data→highlighted)→text→equals(".") then {
          data→highlighted := data→highlighted + 1;
        }
      }
      data→x\_done := true;
      code→changeSpinnerDigit;
    }
    else {
      if($x\_accel < 0.2) and ($x\_accel > - 0.5) then {
        data→x\_done := false;
      }
      else {
        if($x\_accel < - 0.5) and not data→x\_done then {
          data→highlighted := data→highlighted - 1;
          if data→highlighted < 0 then {
            data→highlighted := 0;
            phone→vibrate(0.1);
          }
          else {
            if data→digitsSpriteSet→at(data→highlighted)→text→equals(".") then {
              data→highlighted := data→highlighted - 1;
            }
          }
          data→x\_done := true;
          code→changeSpinnerDigit;
        }
        else {
          if $x\_accel > - 0.2 then {
            data→x\_done := false;
          }
        }
      }
    }
  }
  meta private;
}

action assertHighlightedPos() {
  foreach c1 in records→Counters_table where true do {
    if $c1→highlightedPos→get = $c1→digits→get then {
      $c1→highlightedPos→set(0);
      time→log($c1→Name→get ∥ " highlighted reseted");
    }
  }
  meta private;
}

action mailSetting() {
  wall→push_new_page;
  $s := wall→ask_string("Enter the email address to be used to sens the backup emails:");
  data→mailToAddress := $s;
  wall→pop_page;
  meta private;
}

action explodeDateTime() {
  // Explodes the dateTime data stores in the index in year month day hour minute
  foreach c in records→counterValIndex_index where true do {
    if $c→day→get = 0 then {
      $dt := $c→dt\_val→get;
      $c→year→set($dt→year);
      $c→month→set($dt→month);
      $c→day→set($dt→day);
      $c→hour→set($dt→hour);
      $c→minute→set($dt→minute);
      skip;
    }
  }
}

action CreateSelectorBoard(stringsCollect: String_Collection, aSelectorType: String) {
  data→selectorType := $aSelectorType;
  data→selectorStrCollect := $stringsCollect;
  data→SelectorBoard := media→create_board(640);
  data→selectorSpriteSet := data→SelectorBoard→create_sprite_set;
  $tile\_width := data→SelectorBoard→width * 0.8;
  $tileHeight := data→SelectorBoard→height / 8;
  $tileGap := $tileHeight / 10;
  for 0 ≤ i < $stringsCollect→count do {
    $pic := media→create_picture($tile\_width, $tileHeight);
    $sprite := data→SelectorBoard→create_picture($pic);
    $sprite→set_x(data→SelectorBoard→width / 2);
    $sprite→set_y(($tileHeight + $tileGap) * $i + ($tileHeight / 2) + $tileGap);
    $pic→fill_rect(0, 0, $tile\_width, $tileHeight, 0, colors→orange);
    $pic→draw_text($tileGap, 22, $stringsCollect→at($i), 30, 0, colors→white);
    $sprite→set_color(colors→blue);
    $sprite→set_picture($pic);
    data→selectorSpriteSet→add($sprite);
  }
  data→SelectorBoard→post_to_wall;
  meta private;
}
var selectorType : String {
}

action SelectCounter(index_in_set: Number) {
  foreach c in records→Counters_table where $c→Name→get→equals(data→colCounterNames→at($index_in_set)) do {
    data→selectedCounter := $c;
    data→counterSelectedAction;
    code→CounterPage;
  }
  meta private;
}

action CounterPropPage() {
  wall→set_subtitle("make news propretie");
  $strings := collections→create_string_collection;
  "Stat interval";
  "Stat interval";
  $strings→add("Digits");
  $strings→add("Decimals");
  $strings→add("Comment");
  $strings→add("Stat interval");
  code→CreateSelectorBoard($strings, "CountersProps");
  data→selectedCounter→post_to_wall;
  meta private;
}

action counterPropTapped(index: Number) {
  $s := data→selectorStrCollect→at($index);
  if $s→to_lower_case→equals("stat interval") then {
    code→setStatIntervProp;
  }
  else {
    if $s→to_lower_case→equals("comment") then {
      code→setCommentProp;
    }
    else {
      if $s→to_lower_case→equals("digits") then {
        $x2 := wall→ask_number("Digits number");
        data→selectedCounter→digits→set($x2);
      }
      else {
        if $s→to_lower_case→equals("decimals") then {
          $x1 := wall→ask_number("Decimals number");
          data→selectedCounter→decimals→set($x1);
        }
      }
    }
  }
  wall→pop_page;
  meta private;
}
var selectorStrCollect : String_Collection {
}

action setStatIntervProp() {
  $x := wall→ask_number("Stat interval in seconds\n1h=3600s\n1day=86400s\n1week=604800s\n1year=31536000s\n\n");
  data→selectedCounter→xStatInterval→set($x);
  meta private;
}

action setCommentProp() {
  $b := wall→ask_boolean("?", "allow comments?");
  data→selectedCounter→bCommentEnabled→set($b);
  meta private;
}

action PushCounterStatsPage() {
  $dt\_prev := invalid→datetime;
  $val\_prev := invalid→number;
  $strings := collections→create_string_collection;
  wall→push_new_page;
  wall→set_title(data→selectedCounter→Name→get);
  wall→set_subtitle("Statistics");
  wall→add_button("Back", "Spinner");
  code→computeStats1($dt\_prev, $val\_prev, $strings);
  foreach s in $strings where true do {
    $s→post_to_wall;
  }
  $d := collections→create_string_map;
  code→computeStats2($dt\_prev, 0, $d);
  foreach s1 in $d where true do {
    $s1→post_to_wall;
  }
  meta private;
}

action computeStats1(dt\_prev: DateTime, val\_prev: Number, strings: String_Collection) {
  foreach c in records→counterValIndex_index where $c→counterName→equals(data→selectedCounter→Name→get) do {
    $dt := $c→dt\_val→get;
    $val := $c→val→get;
    if $dt\_prev→is_invalid then {
      skip;
    }
    else {
      $x := $dt→subtract($dt\_prev);
      if $x ≥ data→selectedCounter→xStatInterval→get then {
        $x1 := ($val - $val\_prev) / ($x / data→selectedCounter→xStatInterval→get);
        $s := "From " ∥ $dt\_prev→to_string ∥ " to " ∥ $dt→to_string ∥ " : " ∥ $x1→to_string;
        $strings→add($s);
      }
    }
    $dt\_prev := $dt;
    $val\_prev := $val;
    skip;
  }
  meta private;
}

action computeStats2(dt\_prev: DateTime, val\_prev: Number, strings: String_Map) {
  $curr\_year := 0;
  $curr\_month := 0;
  $total := 0;
  foreach c in records→counterValIndex_index where $c→counterName→equals(data→selectedCounter→Name→get) do {
    $y := $c→year→get;
    $m := $c→month→get;
    $val := $c→val→get;
    if $y > $curr\_year then {
      $curr\_year := $y;
    }
    else {
      if $m > $curr\_month then {
        if $curr\_month > 0 then {
          $s1 := $curr\_year→to_string ∥ "." ∥ $curr\_month→to_string;
          $strings→set_at($s1, $total→to_string);
        }
        $curr\_month := $m;
        $val\_prev := $val;
        $total := 0;
      }
      else {
        $total := $total + ($val - $val\_prev);
      }
    }
    $val\_prev := $val;
    skip;
  }
  $strings→post_to_wall;
}

action aboutpage() {
  wall→push_new_page;
  wall→create_text_box("Home Counters\nthe apps that helps you manage your consumption of electricity, water and other to take home. With the option to connect the accelerometer to be able to make your registration with one hand.\n", 24)→post_to_wall;
  wall→prompt("Home Counters vers 1.13");
  wall→pop_page;
  meta private;
}

action CounterPageold() {
  wall→push_new_page;
  wall→set_title(data→selectedCounter→Name→get);
  wall→set_title("página do contador");
  wall→add_button("add", "add");
  wall→add_button("edit", "edit");
  wall→add_button("delete", "delete");
  wall→add_button("next", "graphics");
  meta private;
}

