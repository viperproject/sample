meta version "v2.2,nothing";
meta name "Sudoku Shapes";
meta icon "ninecolumn";
meta color "#ffe25822";
// Sudoku with shapes and colors instead of numbers. Each pseudo-random puzzle is guaranteed to have one unique (and difficult!) solution. The game is always saved, so you can start and stop playing at your leisure.
//
// Check out my other games by visiting my user profile page on touchdevelop.com or tapping my user name on the details page of any of my scripts in the TouchDevelop app.
meta platform "current";
meta import colorHelpers {
  pub "wszv"
  usage {
    action color_to_number(c: Color) returns res: Number
  }
}

action main() {
  code→gameInit;
  code→gameLoad;
  code→gameDraw;
}

action arrayInsertJson(@in: Json_Object, at: Number, value: Json_Object) returns out: Json_Object {
  $out := code→arrayInsertValue($@in, $at, code→serializeJson($value));
  meta private;
}

action arrayInsertString(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $out := code→arrayInsertValue($@in, $at, code→serializeString($value));
  meta private;
}

action arrayInsertValue(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("[");
  $array := code→newStringCollection;
  for 0 ≤ i < $@in→count do {
    if $i = $at then {
      $array→add($value);
    }
    $array→add(code→serializeJson($@in→at($i)));
  }
  if $at = $@in→count then {
    $array→add($value);
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $out := web→json($strings→join(""));
  meta private;
}

action arrayRemoveAt(@in: Json_Object, at: Number) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("[");
  $array := code→newStringCollection;
  for 0 ≤ i < $@in→count do {
    if $i ≠ $at then {
      $array→add(code→serializeJson($@in→at($i)));
    }
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $out := web→json($strings→join(""));
  meta private;
}

action newArray() returns json: Json_Object {
  $json := web→json("[]");
  meta private;
}

action newObject() returns json: Json_Object {
  $json := web→json("{}");
  meta private;
}

action newStringCollection() returns strings: String_Collection {
  $strings := "" →split(",");
  $strings→clear;
  meta private;
}

action objectAddJson(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object {
  $out := code→objectAddValue($@in, $name, code→serializeJson($value));
  meta private;
}

action objectAddString(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $out := code→objectAddValue($@in, $name, code→serializeString($value));
  meta private;
}

action objectAddValue(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $object→add($@in→keys→at($k) ∥ ":" ∥ code→serializeJson($@in→field($@in→keys→at($k))));
  }
  $object→add($name ∥ ":" ∥ $value);
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action objectRemoveByName(@in: Json_Object, name: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    if not $n→equals($name) then {
      $object→add($n ∥ ":" ∥ code→serializeJson($@in→field($@in→keys→at($k))));
    }
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action objectRename(@in: Json_Object, oldName: String, newName: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    $v := code→serializeJson($@in→field($n));
    if $n→equals($oldName) then {
      $object→add($newName ∥ ":" ∥ $v);
    }
    else {
      $object→add($n ∥ ":" ∥ $v);
    }
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action serializeArray(json: Json_Object) returns s: String {
  $strings := code→newStringCollection;
  $strings→add("[");
  $array := code→newStringCollection;
  for 0 ≤ i < $json→count do {
    $array→add(code→serializeJson($json→at($i)));
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $s := $strings→join("");
  meta private;
}

action serializeJson(json: Json_Object) returns s: String {
  $s := "null";
  if not $json→is_invalid then {
    if $json→kind→equals("string") then {
      $s := code→serializeString($json→to_string);
    }
    else {
      if $json→kind→equals("object") then {
        $s := code→serializeObject($json);
      }
      else {
        if $json→kind→equals("array") then {
          $s := code→serializeArray($json);
        }
        else {
          if $json→kind→equals("number") then {
            $s := $json→to_number ∥ "";
          }
          else {
            $s := $json→to_boolean ∥ "";
          }
        }
      }
    }
  }
  meta private;
}

action serializeObject(json: Json_Object) returns s: String {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $json→keys→count do {
    $object→add($json→keys→at($k) ∥ ":" ∥ code→serializeJson($json→field($json→keys→at($k))));
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $s := $strings→join("");
  meta private;
}

action serializeString(value: String) returns s: String {
  $strings := code→newStringCollection;
  $strings→add("\"");
  foreach char in $value where true do {
    $x := $char→to_unicode;
    if $x < 32 or $x = 34 or $x = 47 or $x = 94 then {
      $strings→add("\\u" ∥ code→toHex($x));
    }
    else {
      $strings→add($char);
    }
  }
  $strings→add("\"");
  $s := $strings→join("");
  meta private;
}

action toHex(x: Number) returns s: String {
  $strings := "" →split(",");
  $strings→clear;
  while $x > 0 or $strings→count < 4 do {
    $byte := math→mod($x, 16);
    $x := ($x - $byte) / 16;
    if $byte < 10 then {
      $strings→add($byte→to_string);
    }
    else {
      $strings→add(($byte + 55)→to_character);
    }
  }
  $strings→@reverse;
  $s := $strings→join("");
  meta private;
}

action arrayAddValue(@in: Json_Object, value: String) returns out: Json_Object {
  $out := code→arrayInsertValue($@in, $@in→count, $value);
  meta private;
}

action arrayAddString(@in: Json_Object, value: String) returns out: Json_Object {
  $out := code→arrayAddValue($@in, code→serializeString($value));
  meta private;
}

action arrayAddJson(@in: Json_Object, value: Json_Object) returns out: Json_Object {
  $out := code→arrayAddValue($@in, code→serializeJson($value));
  meta private;
}
var classData : Json_Object {
}

action classlibInit() {
  data→classData := code→newObject;
  data→classData := code→objectAddString(data→classData, "type", "classlib");
  data→classData := code→objectAddString(data→classData, "version", "0.7");
  data→classData := code→objectAddJson(data→classData, "definitions", code→newArray());
  meta private;
}

action arrayEditValue(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("[");
  $array := code→newStringCollection;
  for 0 ≤ i < $@in→count do {
    if $i = $at then {
      $array→add($value);
    }
    else {
      $array→add(code→serializeJson($@in→at($i)));
    }
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $out := web→json($strings→join(""));
  meta private;
}

action arrayEditString(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $out := code→arrayEditValue($@in, $at, code→serializeString($value));
  meta private;
}

action arrayEditJson(@in: Json_Object, at: Number, value: Json_Object) returns out: Json_Object {
  $out := code→arrayEditValue($@in, $at, code→serializeJson($value));
  meta private;
}

action objectEditValue(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    $v := code→serializeJson($@in→field($n));
    if $n→equals($name) then {
      $object→add($n ∥ ":" ∥ $value);
    }
    else {
      $object→add($n ∥ ":" ∥ $v);
    }
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action objectEditString(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $out := code→objectEditValue($@in, $name, code→serializeString($value));
  meta private;
}

action objectEditJson(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object {
  $out := code→objectEditValue($@in, $name, code→serializeJson($value));
  meta private;
}

action classlibSetDefs(defs: Json_Object) {
  data→classData := code→objectEditJson(data→classData, "definitions", $defs);
  meta private;
}

action defineClass(name: String) {
  code→defineInheritedClass($name, "");
  meta private;
}

action defineProperty(class: String, propertyName: String, propertyType: String) {
  $prop := code→newObject;
  $prop := code→objectAddString($prop, "type", $propertyType);
  $prop := code→objectAddString($prop, "name", $propertyName);
  $def := code→classlibGetDef($class);
  $props := code→arrayAddJson($def→field("properties"), $prop);
  $def := code→objectEditJson($def, "properties", $props);
  code→classlibSetDef($def);
  meta private;
}

action classlibGetDefs() returns defs: Json_Object {
  $defs := data→classData→field("definitions");
  meta private;
}

action gameInit() {
  $reload := false;
  if data→classData→is_invalid or $reload then {
    code→classlibInit;
    data→forecolorsMap := collections→create_number_map;
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→foreground));
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→magenta));
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→white));
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→black));
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→black));
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→black));
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→background));
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→red));
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→brown));
    data→forecolorsMap→set_at(data→forecolorsMap→count, ♻ colorHelpers→color_to_number(colors→light_gray));
    data→backcolorsMap := collections→create_number_map;
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→background));
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→green));
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→sepia));
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→cyan));
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→yellow));
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→red));
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→foreground));
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→orange));
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→light_gray));
    data→backcolorsMap→set_at(data→backcolorsMap→count, ♻ colorHelpers→color_to_number(colors→blue));
    data→orderedTextMap := " ❁⚔⚓☢☣☠⏰☕⛲";
    code→newGrid;
    data→solved := 0;
  }
  data→landscape := false;
  meta private;
}

action newClass(class: String) returns instance: Json_Object {
  $instance := code→newObject;
  $instance := code→objectAddString($instance, "type", $class);
  $inherits := code→newArray();
  $def := code→classlibGetDef($class);
  $defInherits := $def→string("inherits");
  while not $defInherits→is_empty do {
    $inherits := code→arrayAddString($inherits, $defInherits);
    $defInherits := code→classlibGetDef($defInherits)→string("inherits");
  }
  $instance := code→objectAddJson($instance, "inherits", $inherits);
  $props := $def→field("properties");
  for 0 ≤ i < $props→count do {
    $p := $props→at($i);
    $type := $p→string("type");
    $name := $p→string("name");
    if $type→equals("string") then {
      $instance := code→objectAddString($instance, $name, "");
    }
    else {
      if $type→equals("number") then {
        $instance := code→objectAddValue($instance, $name, 0 ∥ "");
      }
      else {
        if $type→equals("boolean") then {
          $instance := code→objectAddValue($instance, $name, false ∥ "");
        }
        else {
          $instance := code→objectAddValue($instance, $name, "\"null\"");
        }
      }
    }
  }
  meta private;
}

action classSetProperty(instance: Json_Object, property: String, value: String) returns updated: Json_Object {
  $updated := $instance;
  $class := $instance→string("type");
  $defs := code→classlibGetDefs();
  $def := code→newObject();
  $d := 0;
  while $d < $defs→count and $def→keys→count = 0 do {
    $json := $defs→at($d);
    if $json→string("name")→equals($class) then {
      $def := $json;
    }
    else {
      $d := $d + 1;
    }
  }
  time→fail_if_not($def→keys→count > 0);
  $props := $def→field("properties");
  for 0 ≤ i < $props→count do {
    $p := $props→at($i);
    $type := $p→string("type");
    $name := $p→string("name");
    if $name→equals($property) then {
      if $type→equals("string") then {
        $updated := code→objectEditString($instance, $property, $value);
      }
      else {
        $updated := code→objectEditValue($instance, $property, $value);
      }
    }
  }
  meta private;
}

action classGetProperty(instance: Json_Object, property: String) returns value: Json_Object {
  $value := $instance→field($property);
  meta private;
}

action classGetPropertyString(instance: Json_Object, property: String) returns value: String {
  $value := code→classGetProperty($instance, $property) ∥ "";
  meta private;
}

action classGetPropertyNumber(instance: Json_Object, property: String) returns value: Number {
  $value := code→classGetProperty($instance, $property)→to_number;
  meta private;
}

action defineInheritedClass(name: String, inherits: String) {
  $classes := code→classlibGetDefs();
  $class := code→newObject;
  $class := code→objectAddString($class, "type", "class");
  $class := code→objectAddString($class, "name", $name);
  $class := code→objectAddString($class, "inherits", $inherits);
  $props := code→newArray;
  $methods := code→newArray;
  if not $inherits→is_empty then {
    $inherited := code→classlibGetDef($inherits);
    $props := $inherited→field("properties");
    $methods := $inherited→field("methods");
  }
  $class := code→objectAddJson($class, "properties", $props);
  $class := code→objectAddJson($class, "methods", $methods);
  code→classlibSetDefs(code→arrayAddJson($classes, $class));
  meta private;
}

action classlibGetDef(class: String) returns def: Json_Object {
  $classes := code→classlibGetDefs();
  $def := code→newObject();
  $c := 0;
  while $c < $classes→count and $def→keys→count = 0 do {
    $json := $classes→at($c);
    if $json→string("name")→equals($class) then {
      $def := $json;
    }
    else {
      $c := $c + 1;
    }
  }
  meta private;
}

action classlibSetDef(class: Json_Object) {
  $classes := code→classlibGetDefs();
  $name := $class→string("name");
  $c := 0;
  while $c < $classes→count do {
    if $classes→at($c)→string("name")→equals($name) then {
      code→classlibSetDefs(code→arrayEditJson(code→classlibGetDefs, $c, $class));
      $c := 99;
    }
    $c := $c + 1;
  }
  meta private;
}

action classIsType(instance: Json_Object, type: String) returns b: Boolean {
  $b := $instance→string("type")→equals($type);
  if not $b then {
    $inherits := $instance→field("inherits");
    $i := 0;
    while $i < $inherits→count and not $b do {
      $b := $inherits→at($i)→to_string→equals($type);
      $i := $i + 1;
    }
  }
  meta private;
}

action classSetPropertyJson(instance: Json_Object, property: String, value: Json_Object) returns updated: Json_Object {
  $updated := code→classSetProperty($instance, $property, code→serializeJson($value));
  meta private;
}

action gameLoad() {
  $reload := false;
  if data→button→is_invalid or $reload then {
    data→button := media→create_picture(48, 48);
    data→button→draw_ellipse(0, 0, 48, 48, 0, colors→foreground, 3);
  }
  if data→restore→is_invalid or $reload then {
    data→restore := media→icon("restore");
    if colors→is_light_theme then {
      data→restore→colorize(colors→transparent, colors→foreground, 0.65);
    }
  }
  if data→recycle→is_invalid or $reload then {
    data→recycle := media→icon("recycle");
    if colors→is_light_theme then {
      data→recycle→colorize(colors→transparent, colors→foreground, 0.65);
    }
  }
  meta private;
}

action gameDraw() {
  data→board := media→create_full_board;
  data→board→set_background(colors→background);
  data→board→post_to_wall;
  data→hotspots := data→board→create_sprite_set;
  code→newText("SUDOKU SHAPES", colors→foreground, 50, 480, 0, 0);
  code→newRectangle(colors→foreground, 472, 472, 4, 68);
  data→texts := data→board→create_sprite_set;
  data→backs := data→board→create_sprite_set;
  for 0 ≤ row < 9 do {
    for 0 ≤ col < 9 do {
      $x := 8 + 51 * $col + math→floor($col / 3) * 3;
      $y := 72 + 51 * $row + math→floor($row / 3) * 3;
      $sprite1 := code→newRectangle(colors→background, 50, 50, $x, $y);
      data→backs→add($sprite1);
      data→texts→add(code→newText("", colors→foreground, 33, 50, $x, $y));
      code→newHotspot($sprite1);
    }
    data→board→update_on_wall;
  }
  data→selectors := data→board→create_sprite_set;
  for 0 ≤ row1 < 2 do {
    for 0 ≤ col1 < 5 do {
      $x1 := 30 + 84 * $col1;
      $y1 := 552 + 60 * $row1;
      data→selectors→add(code→newRectangle(colors→background, 84, 60, $x1, $y1));
      $ind := $row1 * 5 + $col1;
      $sprite := code→newRectangle(data→backcolorsMap→at($ind)→to_color, 74, 50, $x1 + 5, $y1 + 5);
      code→newText(data→orderedTextMap→at($ind), data→forecolorsMap→at($ind)→to_color, 33, 74, $x1 + 5, $y1 + 5);
      code→newHotspot($sprite);
    }
  }
  code→selectCurrent(0);
  code→newRectangle(colors→chrome, 480, 72, 0, 684);
  code→newButton(data→restore, 174);
  code→newButton(data→recycle, 258);
  data→board→update_on_wall;
  code→updateAll;
  meta private;
}
var board : Board {
}
var hotspots : Sprite_Set {
}

event tap_sprite_in_hotspots(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $index_in_set < 81 then {
    if data→startingGrid→at($index_in_set) = 0 then {
      data→grid→set_at($index_in_set, data→current);
      code→updateCell($index_in_set);
      code→checkGrid;
    }
  }
  else {
    if $index_in_set = 91 then {
      code→revertGrid;
      code→updateAll;
      code→selectCurrent(0);
    }
    else {
      if $index_in_set = 92 then {
        code→newGrid;
        code→updateAll;
        foreach sprite1 in data→selectors where $sprite1→color→equals(colors→foreground) do {
          $sprite1→set_color(colors→background);
        }
        code→selectCurrent(0);
      }
      else {
        code→selectCurrent($index_in_set - 81);
      }
    }
  }
  data→board→update_on_wall;
  meta private;
}

action newText(text: String, color: Color, size: Number, width: Number, left: Number, @top: Number) returns sprite: Sprite {
  $height := 1.5 * $size;
  $sprite := data→board→create_text($width, $height, $size, $text);
  $sprite→set_color($color);
  if data→landscape then {
    $sprite→set_angle(270);
    $sprite→set_pos($@top + $height / 2, data→board→height - ($left + $width / 2));
  }
  else {
    $sprite→set_pos($left + $width / 2, $@top + $height / 2);
  }
  meta private;
}
var landscape : Boolean {
}

action newRectangle(color: Color, width: Number, height: Number, left: Number, @top: Number) returns sprite: Sprite {
  $sprite := data→board→create_rectangle($width, $height);
  $sprite→set_color($color);
  if data→landscape then {
    $sprite→set_angle(270);
    $sprite→set_pos($@top + $height / 2, data→board→height - ($left + $width / 2));
  }
  else {
    $sprite→set_pos($left + $width / 2, $@top + $height / 2);
  }
  meta private;
}

action newPicture(pic: Picture, left: Number, @top: Number) returns sprite: Sprite {
  $sprite := data→board→create_picture($pic);
  if data→landscape then {
    $sprite→set_angle(270);
    $sprite→set_pos($@top + $pic→height / 2, data→board→height - ($left + $pic→width / 2));
  }
  else {
    $sprite→set_pos($left + $pic→width / 2, $@top + $pic→height / 2);
  }
  meta private;
}

action newHotspot(over: Sprite) returns sprite: Sprite {
  $sprite := data→board→create_rectangle($over→width, $over→height);
  $sprite→set_color(colors→transparent);
  $sprite→set_angle($over→angle);
  $sprite→set_pos($over→x, $over→y);
  data→hotspots→add($sprite);
  meta private;
}
var button : Picture {
}

action newButton(image: Picture, pos: Number) {
  $button := code→newPicture(data→button, $pos, 696);
  $pic := code→newPicture($image, 0, 0);
  code→newHotspot($button);
  $pic→move_towards($button, 1);
  meta private;
}
var restore : Picture {
}
var recycle : Picture {
}

action newGrid() {
  code→shuffleText;
  data→startingGrid := math→create_number_map;
  for 0 ≤ i < 81 do {
    data→startingGrid→set_at(data→startingGrid→count, 0);
  }
  data→startingGrid→set_at(0, 5);
  data→startingGrid→set_at(3, 1);
  data→startingGrid→set_at(8, 9);
  data→startingGrid→set_at(11, 8);
  data→startingGrid→set_at(14, 5);
  data→startingGrid→set_at(15, 3);
  data→startingGrid→set_at(18, 1);
  data→startingGrid→set_at(21, 6);
  data→startingGrid→set_at(24, 8);
  data→startingGrid→set_at(31, 7);
  data→startingGrid→set_at(33, 4);
  data→startingGrid→set_at(34, 8);
  data→startingGrid→set_at(36, 8);
  data→startingGrid→set_at(44, 6);
  data→startingGrid→set_at(46, 1);
  data→startingGrid→set_at(47, 4);
  data→startingGrid→set_at(49, 5);
  data→startingGrid→set_at(56, 9);
  data→startingGrid→set_at(59, 1);
  data→startingGrid→set_at(62, 5);
  data→startingGrid→set_at(65, 1);
  data→startingGrid→set_at(66, 5);
  data→startingGrid→set_at(69, 6);
  data→startingGrid→set_at(72, 6);
  data→startingGrid→set_at(77, 2);
  data→startingGrid→set_at(80, 7);
  code→rotate(math→rand(4));
  code→shuffleSquares(math→rand(6), false);
  code→shuffleSquares(math→rand(6), true);
  code→shuffleCells(math→rand(8), false);
  code→shuffleCells(math→rand(8), true);
  code→revertGrid;
  meta private;
}

action revertGrid() {
  data→grid := math→create_number_map;
  data→grid→set_many(data→startingGrid);
  meta private;
}
var textMap : String {
}
var startingGrid : Number_Map {
}
var grid : Number_Map {
}
var selectors : Sprite_Set {
}

action selectCurrent(index: Number) {
  data→selectors→at(data→orderedTextMap→index_of(data→textMap→at(data→current), 0))→set_color(colors→background);
  data→selectors→at($index)→set_color(colors→foreground);
  data→current := data→textMap→index_of(data→orderedTextMap→at($index), 0);
  meta private;
}
var current : Number {
}
var texts : Sprite_Set {
}

action updateAll() {
  for 0 ≤ row < 9 do {
    for 0 ≤ col < 9 do {
      code→updateCell($row * 9 + $col);
    }
    data→board→update_on_wall;
  }
  meta private;
}

action updateCell(index: Number) {
  $x := data→grid→at($index);
  $s := data→textMap→at($x);
  $i := data→orderedTextMap→index_of($s, 0);
  data→texts→at($index)→set_text($s);
  data→texts→at($index)→set_color(data→forecolorsMap→at($i)→to_color);
  data→backs→at($index)→set_color(data→backcolorsMap→at($i)→to_color);
  meta private;
}

action checkGrid() {
  if data→grid→min > 0 then {
    $pass := true;
    for 0 ≤ i < 9 do {
      $row := code→newStringCollection();
      for 0 ≤ i1 < 9 do {
        $x := data→grid→at($i * 9 + $i1);
        if $row→contains($x ∥ "") then {
          $pass := false;
        }
        else {
          $row→add($x ∥ "");
        }
      }
    }
    if $pass then {
      for 0 ≤ i2 < 9 do {
        $col := code→newStringCollection();
        for 0 ≤ i3 < 9 do {
          $x1 := data→grid→at($i3 * 9 + $i2);
          if $col→contains($x1 ∥ "") then {
            $pass := false;
          }
          else {
            $col→add($x1 ∥ "");
          }
        }
      }
    }
    if $pass then {
      for 0 ≤ i4 < 3 do {
        for 0 ≤ i5 < 3 do {
          $square := code→newStringCollection();
          for 0 ≤ i6 < 3 do {
            for 0 ≤ i7 < 3 do {
              $x2 := data→grid→at($i4 * 27 + $i6 * 9 + $i5 * 3 + $i7);
              if $square→contains($x2 ∥ "") then {
                $pass := false;
              }
              else {
                $square→add($x2 ∥ "");
              }
            }
          }
        }
      }
    }
    if $pass then {
      data→solved := data→solved + 1;
      bazaar→post_leaderboard_score(data→solved);
      wall→clear;
      if wall→ask_boolean("PUZZLE SOLVED!", "Start a New Game?") then {
        wall→clear;
        data→board→post_to_wall;
        code→newGrid;
        code→updateAll;
        code→selectCurrent(0);
      }
      else {
        wall→clear;
        data→board→post_to_wall;
      }
    }
  }
  meta private;
}
var solved : Number {
}

action shuffleText() {
  $temp := code→newStringCollection();
  data→textMap := data→orderedTextMap;
  while data→textMap→count > 0 do {
    $s := data→textMap→at(math→rand(data→textMap→count));
    data→textMap := data→textMap→replace($s, "");
    $temp→add($s);
  }
  $temp→remove(" ");
  data→textMap := " " ∥ $temp→join("");
  meta private;
}
var orderedTextMap : String {
}

action rotate(times: Number) {
  for 0 ≤ row < 9 do {
    for 0 ≤ col < 9 do {
      $drow := $row;
      $dcol := $col;
      if $times = 1 then {
        $drow := $col;
        $dcol := 8 - $row;
      }
      else {
        if $times = 2 then {
          $drow := 8 - $row;
          $dcol := 8 - $col;
        }
        else {
          if $times = 3 then {
            $drow := 8 - $col;
            $dcol := $row;
          }
        }
      }
      data→grid→set_at($drow * 9 + $dcol, data→startingGrid→at($row * 9 + $col));
    }
  }
  data→startingGrid := math→create_number_map;
  data→startingGrid→set_many(data→grid);
  meta private;
}

action shuffleSquares(pos: Number, rows: Boolean) {
  $order := math→create_number_map;
  $order→set_at(0, 0);
  $order→set_at(1, 1);
  $order→set_at(2, 2);
  if $pos = 1 then {
    $order→set_at(1, 2);
    $order→set_at(2, 1);
  }
  else {
    if $pos = 2 then {
      $order→set_at(1, 0);
      $order→set_at(0, 1);
    }
    else {
      if $pos = 3 then {
        $order→set_at(0, 1);
        $order→set_at(1, 2);
        $order→set_at(2, 0);
      }
      else {
        if $pos = 4 then {
          $order→set_at(0, 2);
          $order→set_at(1, 0);
          $order→set_at(2, 1);
        }
        else {
          if $pos = 5 then {
            $order→set_at(0, 2);
            $order→set_at(2, 0);
          }
        }
      }
    }
  }
  for 0 ≤ i < 3 do {
    $offset := $order→at($i) * 3;
    for 0 ≤ i1 < 3 do {
      $source := $offset + $i1;
      $destination := $i * 3 + $i1;
      for 0 ≤ other < 9 do {
        if $rows then {
          data→grid→set_at($destination * 9 + $other, data→startingGrid→at($source * 9 + $other));
        }
        else {
          data→grid→set_at($other * 9 + $destination, data→startingGrid→at($other * 9 + $source));
        }
      }
    }
  }
  data→startingGrid := math→create_number_map;
  data→startingGrid→set_many(data→grid);
  meta private;
}

action shuffleCells(seq: Number, rows: Boolean) {
  $flips := math→create_number_map;
  $flips→set_at(0, 0);
  $flips→set_at(1, 0);
  $flips→set_at(2, 0);
  if $seq = 1 then {
    $flips→set_at(0, 1);
  }
  else {
    if $seq = 2 then {
      $flips→set_at(1, 1);
    }
    else {
      if $seq = 3 then {
        $flips→set_at(0, 1);
        $flips→set_at(1, 1);
      }
      else {
        if $seq = 4 then {
          $flips→set_at(2, 1);
        }
        else {
          if $seq = 5 then {
            $flips→set_at(0, 1);
            $flips→set_at(2, 1);
          }
          else {
            if $seq = 6 then {
              $flips→set_at(1, 1);
              $flips→set_at(2, 1);
            }
            else {
              if $seq = 7 then {
                $flips→set_at(0, 1);
                $flips→set_at(1, 1);
                $flips→set_at(2, 1);
              }
            }
          }
        }
      }
    }
  }
  for 0 ≤ i2 < 3 do {
    $flipit := $flips→at($i2) = 1;
    $offset := $i2 * 3;
    $source := math→create_number_map;
    $source→set_at(0, $offset);
    $source→set_at(1, $offset + 1);
    $source→set_at(2, $offset + 2);
    $destination := math→create_number_map;
    $destination→set_many($source);
    if $flipit then {
      $pos := math→rand(6);
      $order := math→create_number_map;
      $order→set_at(0, 0);
      $order→set_at(1, 1);
      $order→set_at(2, 2);
      if $pos = 1 then {
        $order→set_at(1, 2);
        $order→set_at(2, 1);
      }
      else {
        if $pos = 2 then {
          $order→set_at(1, 0);
          $order→set_at(0, 1);
        }
        else {
          if $pos = 3 then {
            $order→set_at(0, 1);
            $order→set_at(1, 2);
            $order→set_at(2, 0);
          }
          else {
            if $pos = 4 then {
              $order→set_at(0, 2);
              $order→set_at(1, 0);
              $order→set_at(2, 1);
            }
            else {
              if $pos = 5 then {
                $order→set_at(0, 2);
                $order→set_at(2, 0);
              }
            }
          }
        }
      }
      for 0 ≤ cell < 3 do {
        $source→set_at($cell, $offset + $order→at($cell));
      }
    }
    for 0 ≤ other < 9 do {
      if $rows then {
        for 0 ≤ row < 3 do {
          data→grid→set_at($destination→at($row) * 9 + $other, data→startingGrid→at($source→at($row) * 9 + $other));
        }
      }
      else {
        for 0 ≤ col < 3 do {
          data→grid→set_at($other * 9 + $destination→at($col), data→startingGrid→at($other * 9 + $source→at($col)));
        }
      }
    }
  }
  data→startingGrid := math→create_number_map;
  data→startingGrid→set_many(data→grid);
  meta private;
}
var backcolorsMap : Number_Map {
}
var forecolorsMap : Number_Map {
}
var backs : Sprite_Set {
}

