meta version "v2.2,nothing";
meta name "game base";
meta icon "flowchart";
meta color "#ff008000";
// This script can be used as the basis of many games and UI driven apps.
//
// Check out all my games and utilities by visiting my user profile page on touchdevelop.com or tapping my user name on the details page of any of my scripts in the TouchDevelop app.
meta platform "current";

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
  $reload := true;
  // Change reload to false before publishing.
  if data→classData→is_invalid or $reload then {
    code→classlibInit;
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
  $reload := true;
  // Change reload to false before publishing.
  if data→button→is_invalid or $reload then {
    data→button := media→create_picture(48, 48);
    data→button→draw_ellipse(0, 0, 48, 48, 0, colors→foreground, 3);
  }
  if data→blank→is_invalid or $reload then {
    data→blank := media→create_picture(48, 48);
  }
  if data→ok→is_invalid or $reload then {
    data→ok := media→icon("check");
    data→ok→resize(44, 47);
    if colors→is_light_theme then {
      data→ok→colorize(colors→transparent, colors→foreground, 0.65);
    }
  }
  if data→cancel→is_invalid or $reload then {
    data→cancel := data→blank→clone;
    data→cancel→draw_text(13, 12, 10060→to_character, 24, 0, colors→foreground);
  }
  meta private;
}

action gameDraw() {
  data→board := media→create_full_board;
  data→board→set_background(colors→background);
  data→board→post_to_wall;
  data→hotspots := data→board→create_sprite_set;
  code→newRectangle(colors→chrome, 480, 72, 0, 656);
  code→newButton(data→ok, 174);
  code→newButton(data→cancel, 258);
  data→status := code→newText("", colors→accent, 40, 480, 0, 0);
  data→board→update_on_wall;
  meta private;
}
var board : Board {
}
var hotspots : Sprite_Set {
}

event tap_sprite_in_hotspots(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $index_in_set = 0 then {
    data→status→set_text("OK");
  }
  else {
    data→status→set_text("Cancel");
  }
  meta private;
}

event gameloop() {
  if time→now→greater(data→timer) then {
    data→status→set_text("");
    data→timer := time→now→add_milliseconds(2500);
  }
  meta private;
}
var timer : DateTime {
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
  $button := code→newPicture(data→button, $pos, 668);
  $pic := code→newPicture($image, 0, 0);
  code→newHotspot($button);
  $pic→move_towards($button, 1);
  meta private;
}
var blank : Picture {
}

action update() {
  skip;
  meta private;
}
var ok : Picture {
}
var cancel : Picture {
}
var status : Sprite {
}

