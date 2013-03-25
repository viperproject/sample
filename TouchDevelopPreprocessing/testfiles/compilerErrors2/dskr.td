meta version "v2.2,nothing";
meta name "Casino Slots";
meta icon "cherry";
meta color "#ffff0038";
// Slot machine. If your bank is below 100, you will get a fresh 100 credit bank roll each time you restart.
//
// Check out my other games by visiting my user profile page on touchdevelop.com or tapping my user name on the details page of any of my scripts in the TouchDevelop app.
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
  $reload := false;
  if data→classData→is_invalid or $reload then {
    code→classlibInit;
    code→defineClass("Payout");
    code→defineProperty("Payout", "Description", "string");
    code→defineProperty("Payout", "Value", "number");
    code→defineProperty("Payout", "Bonus", "number");
    data→payouts := code→newArray();
    code→newPayout("", 0, 0);
    data→zeroPayout := data→payouts→at(0);
    data→currentPayout := data→zeroPayout;
    data→payouts := code→newArray();
    code→newPayout("Cherry", 2, 0);
    code→newPayout("Cherry-cherry", 5, 0);
    code→newPayout("Red-Red-Red", 10, 0);
    code→newPayout("Black-Black-Black", 20, 0);
    code→newPayout("Three Cherries", 30, 0);
    code→newPayout("Three Hearts", 30, 0);
    code→newPayout("Three Clubs", 50, 0);
    code→newPayout("Three Diamonds", 100, 0);
    code→newPayout("Three Spades", 250, 0);
    code→newPayout("Three Wilds", 3000, 1000);
    data→bet := 1;
    data→stopCount := 3;
    data→paylineStop := math→floor(data→stopCount / 2);
    data→payline := math→create_number_map;
    data→payline→set_at(0, 1);
    data→payline→set_at(1, 12);
    data→payline→set_at(2, 9);
    data→imageReel := math→create_number_map;
    $reel := "3020105020406050103040";
    for 0 ≤ i < 22 do {
      data→imageReel→set_at($i, $reel→at($i)→to_number);
    }
    data→weightedReel := math→create_number_map;
    data→weightedReel→set_at(0, 12);
    data→weightedReel→set_at(1, 10);
    data→weightedReel→set_at(2, 20);
    data→weightedReel→set_at(3, 0);
    $j := 4;
    while $j < 6 do {
      data→weightedReel→set_at($j, 18);
      $j := $j + 1;
    }
    while $j < 8 do {
      data→weightedReel→set_at($j, 2);
      $j := $j + 1;
    }
    while $j < 10 do {
      data→weightedReel→set_at($j, 8);
      $j := $j + 1;
    }
    while $j < 13 do {
      data→weightedReel→set_at($j, 4);
      $j := $j + 1;
    }
    while $j < 15 do {
      data→weightedReel→set_at($j, 16);
      $j := $j + 1;
    }
    while $j < 18 do {
      data→weightedReel→set_at($j, 6);
      $j := $j + 1;
    }
    while $j < 21 do {
      data→weightedReel→set_at($j, 14);
      $j := $j + 1;
    }
    data→weightedReel→set_at($j, 1);
    $j := $j + 1;
    data→weightedReel→set_at($j, 9);
    $j := $j + 1;
    data→weightedReel→set_at($j, 13);
    $j := $j + 1;
    data→weightedReel→set_at($j, 15);
    $j := $j + 1;
    data→weightedReel→set_at($j, 17);
    $j := $j + 1;
    data→weightedReel→set_at($j, 19);
    $j := $j + 1;
    data→weightedReel→set_at($j, 21);
    $j := $j + 1;
    while $j < 32 do {
      data→weightedReel→set_at($j, 11);
      $j := $j + 1;
    }
  }
  data→landscape := false;
  if data→bank < 100 or $reload then {
    data→bank := 100;
  }
  data→spinning := false;
  data→picked := math→create_number_map;
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
    $radius := 40;
    $diameter := $radius * 2;
    $length := 120;
    $border := 8;
    data→button := media→create_picture($diameter + $length, $diameter);
    data→button→draw_rect($radius, 0, $length, $diameter, 0, colors→red, $border);
    data→button→fill_ellipse(0, 0, $diameter, $diameter, 0, colors→red);
    data→button→fill_ellipse($length, 0, $diameter, $diameter, 0, colors→red);
    $bd := $diameter - $border * 2;
    data→button→fill_ellipse($border, $border, $bd, $bd, 0, colors→yellow);
    data→button→fill_ellipse($length + $border, $border, $bd, $bd, 0, colors→yellow);
    data→button→fill_rect($radius, $border, $length, $bd, 0, colors→yellow);
  }
  if data→heart→is_invalid or $reload then {
    data→heart := media→large_icon("heartalt");
    data→heart→colorize(colors→transparent, colors→red, 0.65);
  }
  if data→diamond→is_invalid or $reload then {
    data→diamond := media→create_picture(96, 120);
    data→diamond→draw_text(15, 9, "♦", 96, 0, colors→red);
    data→diamond→resize(96, 96);
  }
  if data→club→is_invalid or $reload then {
    data→club := media→large_icon("club");
    data→club→colorize(colors→transparent, colors→black, 0.65);
  }
  if data→spade→is_invalid or $reload then {
    data→spade := media→large_icon("spade");
    data→spade→colorize(colors→transparent, colors→black, 0.65);
  }
  if data→cherry→is_invalid or $reload then {
    data→cherry := media→create_picture(96, 96);
    $pic := art→cherryart→clone;
    $pic→resize(68, 68);
    data→cherry→blend($pic, 14, 14, 0, 1);
  }
  if data→wild→is_invalid or $reload then {
    data→wild := media→create_picture(96, 96);
    data→wild→clear(colors→white);
    $pic1 := data→heart→clone;
    $pic1→resize(36, 36);
    data→wild→blend($pic1, 18, 18, 0, 1);
    $pic1 := data→diamond→clone;
    $pic1→resize(36, 36);
    data→wild→blend($pic1, 42, 42, 0, 1);
    $pic1 := data→club→clone;
    $pic1→resize(36, 36);
    data→wild→blend($pic1, 42, 18, 0, 1);
    $pic1 := data→spade→clone;
    $pic1→resize(36, 36);
    data→wild→blend($pic1, 18, 42, 0, 1);
  }
  if data→blank→is_invalid or $reload then {
    data→blank := media→create_picture(96, 96);
  }
  meta private;
}

action gameDraw() {
  data→board := media→create_full_board;
  data→board→set_background(colors→blue);
  data→board→post_to_wall;
  data→hotspots := data→board→create_sprite_set;
  code→newRectangle(colors→white, 288, 128, 96, 80);
  data→stops := data→board→create_sprite_set;
  for 0 ≤ i < data→payline→count do {
    for 0 ≤ j < data→stopCount do {
      data→stops→add(code→newPicture(code→getStopImage($i, $j), 96 * ($i + 1), 48 * ($j + 1)));
      ... ;
    }
  }
  code→newRectangle(colors→black, 288, 2, 96, 143);
  code→newRectangle(colors→gray, 480, 96, 0, 0);
  code→newRectangle(colors→gray, 480, 192, 0, 192);
  code→newRectangle(colors→gray, 96, 96, 0, 96);
  code→newRectangle(colors→gray, 96, 96, 384, 96);
  code→newPicture(data→heart, 0, 96);
  code→newPicture(data→club, 384, 96);
  code→newPicture(data→spade, 0, 192);
  code→newPicture(data→diamond, 384, 192);
  code→newRectangle(colors→black, 440, 64, 20, 304);
  code→newButton("SPIN", 400);
  code→newButton("BET MAX", 496);
  code→newButton("BET ONE", 592);
  code→newButton("PAYOUT", 688);
  data→banklabel := code→newText("BANK:", colors→yellow, 38, 1, 176, 308);
  data→banktext := code→newText((data→bank ∥ ""), colors→yellow, 38, 180, 272, 308);
  data→banktext→hide;
  data→wonlabel := code→newText("WON:", colors→yellow, 38, 1, 176, 308);
  data→wontext := code→newText((0 ∥ ""), colors→yellow, 38, 180, 272, 308);
  code→newText("BET:", colors→yellow, 38, 1, 44, 308);
  data→bettext := code→newText((data→bet ∥ ""), colors→yellow, 38, 1, 128, 308);
  data→lastSpin := code→newText("", colors→yellow, 30, 480, 0, 240);
  code→update(false);
  data→board→update_on_wall;
  data→payoutSprites := data→board→create_sprite_set;
  data→payoutSprites→add(code→newRectangle(colors→blue, 480, 800, 0, 0));
  $descr := code→newStringCollection();
  $payout1 := code→newStringCollection();
  $payout2 := code→newStringCollection();
  $payout3 := code→newStringCollection();
  foreach payout in data→payouts where true do {
    $descr→add(code→classGetPropertyString($payout, "Description"));
    $v := code→classGetPropertyNumber($payout, "Value");
    $payout1→add($v ∥ "");
    $payout2→add($v * 2 ∥ "");
    $b := code→classGetPropertyNumber($payout, "Bonus");
    $payout3→add($v * 3 + $b ∥ "");
  }
  $descr→add("");
  $payout1→add("1");
  $payout2→add(2 ∥ "");
  $payout3→add(3 ∥ "");
  $descr→@reverse;
  $payout1→@reverse;
  $payout2→@reverse;
  $payout3→@reverse;
  data→payoutSprites→add(code→newText($descr→join("\n"), colors→yellow, 24, 220, 6, 240));
  data→payoutSprites→add(code→newText($payout1→join("\n"), colors→yellow, 24, 80, 226, 240));
  data→payoutSprites→add(code→newText($payout2→join("\n"), colors→yellow, 24, 80, 306, 240));
  data→payoutSprites→add(code→newText($payout3→join("\n"), colors→yellow, 24, 80, 386, 240));
  data→payoutSprites→add(code→newText("PAYOUT TABLE", colors→yellow, 40, 480, 0, 92));
  code→newText("CASINO SLOTS", colors→yellow, 64, 480, 0, 0);
  code→showPayout(false);
  data→board→update_on_wall;
  meta private;
}
var board : Board {
}
var hotspots : Sprite_Set {
}

event tap_sprite_in_hotspots(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if not data→spinning then {
    if $index_in_set = 0 then {
      code→spin;
    }
    else {
      if $index_in_set = 3 then {
        code→showPayout(true);
      }
      else {
        if $index_in_set = 1 then {
          data→bet := 3;
        }
        else {
          if $index_in_set = 2 then {
            data→bet := math→mod(data→bet, 3) + 1;
          }
        }
        data→bettext→set_text(data→bet ∥ "");
      }
    }
  }
  meta private;
}

event gameloop() {
  if data→spinning then {
    if time→now→greater(data→spintimer) and data→stops→at(data→reel * data→stopCount + data→paylineStop)→y ≥ 130 then {
      code→score;
      data→spintimer := time→now→add_milliseconds(500);
    }
    foreach stop in data→stops where $stop→y > 240 do {
      $stop→move(0, - 192);
      $reel := math→floor(data→stops→index_of($stop) / data→stopCount);
      $x := math→mod(data→payline→at($reel) + 1, data→imageReel→count);
      data→payline→set_at($reel, $x);
      $stop→set_picture(code→getStopImage($reel, 0));
    }
    data→board→evolve;
    data→board→update_on_wall;
  }
  else {
    if time→now→greater(data→displaytimer) then {
      if data→banktext→is_visible then {
        data→banktext→hide;
        data→banklabel→hide;
        data→wontext→show;
        data→wonlabel→show;
      }
      else {
        data→banktext→show;
        data→banklabel→show;
        data→wontext→hide;
        data→wonlabel→hide;
      }
      data→board→update_on_wall;
      data→displaytimer := time→now→add_milliseconds(2500);
    }
  }
  meta private;
}
var displaytimer : DateTime {
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
var cherryart : Picture {
  is\_resource = true;
  url = "http://www.wpclipart.com/food/fruit/cherry/cherries.png";
}
var cherry : Picture {
}
var heart : Picture {
}
var club : Picture {
}
var diamond : Picture {
}
var spade : Picture {
}
var wild : Picture {
}

action newButton(text: String, pos: Number) {
  $button := code→newPicture(data→button, 140, $pos);
  $label := code→newText($text, colors→red, 40, 200, 0, 0);
  code→newHotspot($button);
  $label→move_towards($button, 1);
  meta private;
}
var blank : Picture {
}
var weightedReel : Number_Map {
}
var imageReel : Number_Map {
}
var payline : Number_Map {
}
var stops : Sprite_Set {
}
var stopCount : Number {
}

action getStopImage(reel: Number, stop: Number) returns pic: Picture {
  $imageIndex := math→mod(data→payline→at($reel) + $stop - data→paylineStop + data→imageReel→count, data→imageReel→count);
  $image := data→imageReel→at($imageIndex);
  $pic := data→blank;
  if $image = 0 then {
    skip;
  }
  else {
    if $image = 1 then {
      $pic := data→heart;
    }
    else {
      if $image = 2 then {
        $pic := data→club;
      }
      else {
        if $image = 3 then {
          $pic := data→diamond;
        }
        else {
          if $image = 4 then {
            $pic := data→spade;
          }
          else {
            if $image = 5 then {
              $pic := data→cherry;
            }
            else {
              if $image = 6 then {
                $pic := data→wild;
              }
            }
          }
        }
      }
    }
  }
  meta private;
}
var paylineStop : Number {
}
var bet : Number {
}
var bank : Number {
}
var bettext : Sprite {
}
var banklabel : Sprite {
}
var banktext : Sprite {
}
var wonlabel : Sprite {
}
var wontext : Sprite {
}
var payouts : Json_Object {
}

action newPayout(description: String, value: Number, bonus: Number) {
  $payout := code→newClass("Payout");
  $payout := code→classSetProperty($payout, "Description", $description→to_upper_case);
  $payout := code→classSetProperty($payout, "Value", $value ∥ "");
  $payout := code→classSetProperty($payout, "Bonus", $bonus ∥ "");
  data→payouts := code→arrayAddJson(data→payouts, $payout);
  meta private;
}
var payoutSprites : Sprite_Set {
}

action showPayout(show: Boolean) {
  foreach sprite in data→payoutSprites where $show do {
    $sprite→show;
  }
  foreach sprite1 in data→payoutSprites where not $show do {
    $sprite1→hide;
  }
  meta private;
}

event tap_sprite_in_payoutSprites(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  code→showPayout(false);
  meta private;
}

action spin() {
  if data→bet ≤ data→bank then {
    data→bank := data→bank - data→bet;
    data→banktext→set_text(data→bank ∥ "");
    data→lastSpin→set_text("");
    data→banklabel→show;
    data→banktext→show;
    data→wonlabel→hide;
    data→wontext→hide;
    data→spinning := true;
    data→reel := 0;
    for 0 ≤ i < 3 do {
      data→picked→set_at($i, data→weightedReel→at(math→rand(data→weightedReel→count)));
    }
    foreach stop in data→stops where true do {
      $stop→set_speed_y(480);
    }
    data→spintimer := time→now→add_seconds(2);
  }
  meta private;
}
var spinning : Boolean {
}
var picked : Number_Map {
}
var spintimer : DateTime {
}
var reel : Number {
}

action score() {
  data→payline→set_at(data→reel, data→picked→at(data→reel));
  for 0 ≤ s < data→stopCount do {
    $sprite := data→stops→at(data→reel * data→stopCount + $s);
    $sprite→set_speed_y(0);
    $sprite→set_y(48 * ($s + 2));
    data→board→update_on_wall;
    $sprite→set_picture(code→getStopImage(data→reel, $s));
  }
  data→board→update_on_wall;
  data→reel := data→reel + 1;
  if data→reel = 3 then {
    data→spinning := false;
    $values := code→newStringCollection();
    $matches := math→create_number_map;
    $cherry := 3;
    for 0 ≤ r < 3 do {
      $x := data→imageReel→at(data→picked→at($r));
      if $x = 0 then {
        $x := ($r + 1) * 10;
      }
      if $x = 5 then {
        $cherry := $r;
      }
      if $values→contains($x ∥ "") then {
        $matches→set_at($matches→count, $x);
      }
      else {
        $values→add($x ∥ "");
      }
    }
    $payout := data→zeroPayout;
    if $values→contains(5 ∥ "") then {
      if $values→count = 3 then {
        if $cherry = 0 then {
          $payout := data→payouts→at(0);
        }
      }
      else {
        if $values→count = 2 then {
          if $values→at(0)→equals(5 ∥ "") then {
            $payout := data→payouts→at(0);
          }
          if $matches→at(0) = 5 then {
            if $cherry = 1 then {
              $payout := data→payouts→at(1);
            }
          }
        }
        else {
          $payout := data→payouts→at(4);
        }
      }
    }
    else {
      while $values→remove(6 ∥ "") do {
        skip;
      }
      if $values→count = 3 then {
        skip;
      }
      else {
        if $values→count = 2 then {
          if $values→contains(1 ∥ "") and $values→contains(3 ∥ "") then {
            $payout := data→payouts→at(2);
          }
          else {
            if $values→contains(2 ∥ "") and $values→contains(4 ∥ "") then {
              $payout := data→payouts→at(3);
            }
          }
        }
        else {
          if $values→count = 1 then {
            if $matches→at(0) = 6 then {
              $payout := data→payouts→at($values→at(0)→to_number + 4);
            }
            else {
              $payout := data→payouts→at($matches→at(0) + 4);
            }
          }
          else {
            $payout := data→payouts→at(9);
          }
        }
      }
    }
    data→currentPayout := $payout;
    code→update(true);
  }
  meta private;
}
var zeroPayout : Json_Object {
}
var currentPayout : Json_Object {
}
var lastSpin : Sprite {
}

action update(addScore: Boolean) {
  $s := code→classGetPropertyString(data→currentPayout, "Description");
  data→lastSpin→set_text($s);
  $x := code→classGetPropertyNumber(data→currentPayout, "Value") * data→bet;
  if data→bet = 3 then {
    $x := $x + code→classGetPropertyNumber(data→currentPayout, "Bonus");
  }
  data→wontext→set_text($x ∥ "");
  if $addScore then {
    data→bank := data→bank + $x;
    bazaar→post_leaderboard_score(data→bank);
  }
  data→banktext→set_text(data→bank ∥ "");
  meta private;
}

action go() {
  skip;
}

action go2() {
  skip;
}

