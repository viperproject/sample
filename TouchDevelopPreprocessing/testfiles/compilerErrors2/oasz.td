meta version "v2.2,nothing";
meta name "Jacks or Better";
meta icon "club";
meta color "#ff008000";
// Classic Jacks or Better video poker. The first time you run this script, allow time for the images to load.
meta platform "current";

action main() {
  code→init;
  code→load;
  code→draw;
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

action init() {
  if data→classData→is_invalid or true then {
    code→classlibInit;
    code→defineClass("deck");
    code→defineProperty("deck", "dealt", "array(card)");
    code→defineClass("hand");
    code→defineProperty("hand", "cards", "array(card)");
    code→defineProperty("hand", "score", "number");
    code→defineProperty("hand", "hand", "string");
    code→defineClass("card");
    code→defineProperty("card", "value", "number");
    code→defineProperty("card", "rank", "string");
    code→defineProperty("card", "suit", "string");
  }
  data→deck := code→newClass("deck");
  if data→bank < 100 then {
    data→bank := 100;
  }
  if data→bet = 0 then {
    data→bet := 1;
  }
  data→discarding := false;
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

action deckDeal() returns card: Json_Object {
  $card := code→cardFromNumber(math→rand(52));
  $dealt := code→classGetProperty(data→deck, "dealt");
  $b := true;
  while $b do {
    $b := false;
    for 0 ≤ i < $dealt→count do {
      if not $b then {
        $b := code→cardEquals($card, $dealt→at($i));
      }
    }
    if $b then {
      $card := code→cardFromNumber(math→rand(52));
    }
  }
  $dealt := code→arrayAddJson($dealt, $card);
  data→deck := code→classSetPropertyJson(data→deck, "dealt", $dealt);
  meta private;
}

action classSetPropertyJson(instance: Json_Object, property: String, value: Json_Object) returns updated: Json_Object {
  $updated := code→classSetProperty($instance, $property, code→serializeJson($value));
  meta private;
}

action deckShuffle() {
  data→deck := code→classSetPropertyJson(data→deck, "dealt", code→newArray);
  meta private;
}

action cardFromNumber(n: Number) returns card: Json_Object {
  $card := code→newClass("card");
  $card := code→classSetProperty($card, "value", $n ∥ "");
  $suit := math→mod($n, 4);
  $rank := math→mod($n, 13);
  $card := code→classSetProperty($card, "suit", "HCDS" →at($suit));
  if $rank = 12 then {
    $card := code→classSetProperty($card, "rank", 10 ∥ "");
  }
  else {
    $card := code→classSetProperty($card, "rank", "A23456789JQK" →at($rank));
  }
  meta private;
}

action cardEquals(card: Json_Object, other: Json_Object) returns equals: Boolean {
  $equals := code→classGetPropertyNumber($card, "value") = code→classGetPropertyNumber($other, "value");
  meta private;
}
var deck : Json_Object {
}

action deal() {
  data→bank := data→bank - data→bet;
  data→banktext→set_text(data→bank ∥ "");
  data→banktext→show;
  data→banklabel→show;
  data→wontext→hide;
  data→wonlabel→hide;
  data→handDisplay→hide;
  for 0 ≤ i < 5 do {
    $y := 4 - $i;
    code→showCard($y, false);
    data→cardSprites→at($y * 5 + 3)→hide;
    data→board→update_on_wall;
  }
  code→deckShuffle;
  data→hand := code→newClass("hand");
  $cards := code→newArray();
  for 0 ≤ i1 < 5 do {
    $x := 4 - $i1;
    $card := code→deckDeal;
    $cards := code→arrayAddJson($cards, $card);
    code→update($card, $x);
  }
  data→hand := code→classSetPropertyJson(data→hand, "cards", $cards);
  data→dealdraw→set_text("DRAW");
  data→discarding := true;
  meta private;
}
var hand : Json_Object {
}

action load() {
  if data→heart→is_invalid then {
    data→heart := media→large_icon("heartalt");
    data→heart→colorize(colors→white, colors→red, 0.65);
  }
  if data→diamond→is_invalid then {
    data→diamond := media→create_picture(96, 120);
    data→diamond→clear(colors→white);
    data→diamond→draw_text(15, 9, "♦", 96, 0, colors→red);
    data→diamond→resize(96, 96);
  }
  if data→club→is_invalid then {
    data→club := media→large_icon("club");
    data→club→colorize(colors→white, colors→black, 0.65);
  }
  if data→spade→is_invalid then {
    data→spade := media→large_icon("spade");
    data→spade→colorize(colors→white, colors→black, 0.65);
  }
  if data→select→is_invalid then {
    data→select := media→create_picture(142, 202);
    $c := colors→from_argb(1, 1, 0.5, 0);
    data→select→draw_rect(0, 0, data→select→width, data→select→height, 0, $c, 2);
    data→select→draw_text(40, 48, "HELD", 24, 0, colors→brown);
    data→select→fill_rect(0, 0, data→select→width, data→select→height, 0, colors→from_argb(0.2, 1, 1, 0));
  }
  if data→back→is_invalid then {
    data→back := media→create_picture(120, 180);
    $radius := 15;
    $diameter := $radius * 2;
    $c1 := colors→black;
    for 0 ≤ i < 6 do {
      for 0 ≤ i1 < 4 do {
        if $c1→equals(colors→black) then {
          $c1 := colors→red;
        }
        else {
          $c1 := colors→black;
        }
        data→back→fill_ellipse($i1 * $diameter, $i * $diameter, $radius, $radius, 0, $c1);
        if $c1→equals(colors→black) then {
          $c1 := colors→red;
        }
        else {
          $c1 := colors→black;
        }
        data→back→fill_ellipse($i1 * $diameter + $radius, $i * $diameter + $radius, $radius, $radius, 0, $c1);
      }
    }
  }
  if data→card→is_invalid then {
    data→card := media→create_picture(132, 192);
    $radius1 := 8;
    $diameter1 := $radius1 * 2;
    data→card→fill_ellipse(0, 0, $diameter1, $diameter1, 0, colors→white);
    data→card→fill_ellipse(data→card→width - $diameter1, 0, $diameter1, $diameter1, 0, colors→white);
    data→card→fill_ellipse(data→card→width - $diameter1, data→card→height - $diameter1, $diameter1, $diameter1, 0, colors→white);
    data→card→fill_ellipse(0, data→card→height - $diameter1, $diameter1, $diameter1, 0, colors→white);
    data→card→fill_rect($radius1, 0, data→card→width - $diameter1, data→card→height, 0, colors→white);
    data→card→fill_rect(0, $radius1, $diameter1, data→card→height - $diameter1, 0, colors→white);
    data→card→fill_rect(data→card→width - $diameter1, $radius1, $diameter1, data→card→height - $diameter1, 0, colors→white);
  }
  meta private;
}
var heart : Picture {
}
var diamond : Picture {
}
var club : Picture {
}
var spade : Picture {
}
var card : Picture {
}

action draw() {
  data→board := media→create_full_board;
  data→board→set_background(colors→from_rgb(0, 0.4, 0));
  data→board→post_to_wall;
  data→cardSprites := data→board→create_sprite_set;
  data→hotspots := data→board→create_sprite_set;
  for 0 ≤ i < 5 do {
    $card := data→board→create_picture(data→card);
    $card→set_pos($card→width - 10, ($card→width + 24) * ($i + 0.5) + 10);
    $card→set_angle(270);
    $suit := data→board→create_picture(data→heart);
    $suit→move_towards($card, 1);
    $suit→set_angle(270);
    data→cardSprites→add($suit);
    $rank := data→board→create_text(60, 60, 40, "");
    $rank→set_color(colors→red);
    $rank→move_towards($card, 1);
    $rank→move(($card→height - $rank→height) / - 1.825, ($card→width - $rank→width) / 1.825);
    $rank→set_angle(270);
    data→cardSprites→add($rank);
    $rank := data→board→create_text(60, 60, 40, "");
    $rank→set_color(colors→red);
    $rank→move_towards($card, 1);
    $rank→move(($card→height - $rank→height) / 1.825, ($card→width - $rank→width) / - 1.825);
    $rank→set_angle(270);
    data→cardSprites→add($rank);
    $select := data→board→create_picture(data→select);
    $select→set_angle(270);
    $select→move_towards($card, 1);
    $select→hide;
    data→cardSprites→add($select);
    $cardback := data→board→create_picture(data→back);
    $cardback→set_angle(270);
    $cardback→move_towards($card, 1);
    data→cardSprites→add($cardback);
    code→showCard($i, false);
    data→board→update_on_wall;
    $hotspot := data→board→create_rectangle($select→width, $select→height);
    $hotspot→set_color(colors→transparent);
    $hotspot→set_angle(270);
    $hotspot→move_towards($card, 1);
    data→hotspots→add($hotspot);
  }
  code→button("DEAL", 100);
  code→button("BET MAX", 300);
  code→button("BET ONE", 500);
  code→button("PAYOUT", 700);
  $back := data→board→create_rectangle(80, 590);
  $back→set_color(colors→black);
  $back→set_pos(318, 400);
  $label := data→board→create_text(1, 81, 54, "BET:");
  $label→set_color(colors→yellow);
  $label→set_pos(318, 685);
  $label→set_angle(270);
  data→bettext := data→board→create_text(1, 81, 54, data→bet ∥ "");
  data→bettext→set_color(colors→yellow);
  data→bettext→set_pos(318, 560);
  data→bettext→set_angle(270);
  $label := data→board→create_text(1, 81, 54, "BANK:");
  $label→set_color(colors→yellow);
  $label→set_pos(318, 460);
  $label→set_angle(270);
  data→banklabel := $label;
  data→banktext := data→board→create_text(300, 81, 54, data→bank ∥ "");
  data→banktext→set_color(colors→yellow);
  data→banktext→set_pos(318, 200);
  data→banktext→set_angle(270);
  data→banktext→hide;
  $label := data→board→create_text(1, 81, 54, "WON:");
  $label→set_color(colors→yellow);
  $label→set_pos(318, 460);
  $label→set_angle(270);
  data→wonlabel := $label;
  data→wontext := data→board→create_text(300, 81, 54, data→won ∥ "");
  data→wontext→set_color(colors→yellow);
  data→wontext→set_pos(318, 200);
  data→wontext→set_angle(270);
  $label := data→board→create_text(data→board→height, 60, 40, "");
  $label→set_color(colors→yellow);
  $label→set_pos(250, 400);
  $label→set_angle(270);
  data→handDisplay := $label;
  data→payoutScreen := data→board→create_sprite_set;
  $sprite := data→board→create_rectangle(data→board→width, data→board→height);
  $sprite→set_color(colors→black);
  data→payoutScreen→add($sprite);
  $sprite := data→board→create_text(800, 60, 40, "JACKS OR BETTER PAYOUT TABLE");
  $sprite→set_color(colors→yellow);
  $sprite→set_angle(270);
  $sprite→set_pos(30, 400);
  data→payoutScreen→add($sprite);
  $x := 30;
  $x1 := $x * 1.5;
  $sprite := data→board→create_text(400, $x1, $x, "\nROYAL FLUSH\nSTRAIGHT FLUSH\nFOUR OF A KIND\nFULL HOUSE\nFLUSH\nSTRAIGHT\nTHREE OF A KIND\nTWO PAIR\nJACKS OR BETTER");
  $sprite→set_color(colors→yellow);
  $sprite→set_angle(270);
  $sprite→set_pos(100, 600);
  data→payoutScreen→add($sprite);
  $sprite := data→board→create_text(80, $x1, $x, "1\n250\n50\n25\n9\n6\n4\n3\n2\n1");
  $sprite→set_color(colors→yellow);
  $sprite→set_angle(270);
  $sprite→set_pos(100, 440);
  data→payoutScreen→add($sprite);
  $sprite := data→board→create_text(80, $x1, $x, "2\n500\n100\n50\n18\n12\n8\n6\n4\n2");
  $sprite→set_color(colors→yellow);
  $sprite→set_angle(270);
  $sprite→set_pos(100, 360);
  data→payoutScreen→add($sprite);
  $sprite := data→board→create_text(80, $x1, $x, "3\n750\n150\n75\n27\n18\n12\n9\n6\n3");
  $sprite→set_color(colors→yellow);
  $sprite→set_angle(270);
  $sprite→set_pos(100, 280);
  data→payoutScreen→add($sprite);
  $sprite := data→board→create_text(80, $x1, $x, "4\n1000\n200\n100\n36\n24\n16\n12\n8\n4");
  $sprite→set_color(colors→yellow);
  $sprite→set_angle(270);
  $sprite→set_pos(100, 200);
  data→payoutScreen→add($sprite);
  $sprite := data→board→create_text(80, $x1, $x, "5\n4000\n250\n125\n45\n30\n20\n15\n10\n5");
  $sprite→set_color(colors→yellow);
  $sprite→set_angle(270);
  $sprite→set_pos(100, 120);
  data→payoutScreen→add($sprite);
  code→showPayout(false);
  data→board→update_on_wall;
  meta private;
}
var cardSprites : Sprite_Set {
}
var board : Board {
}
var select : Picture {
}
var hotspots : Sprite_Set {
}

event tap_sprite_in_hotspots(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $index_in_set < 5 then {
    if data→discarding then {
      $select := data→cardSprites→at($index_in_set * 5 + 3);
      if $select→is_visible then {
        $select→hide;
      }
      else {
        $select→show;
      }
    }
  }
  else {
    if $index_in_set = 5 then {
      if data→discarding then {
        code→score;
      }
      else {
        if data→bank ≥ data→bet then {
          code→deal;
        }
      }
    }
    else {
      if $index_in_set = 6 then {
        data→bet := 5;
      }
      else {
        if $index_in_set = 7 then {
          data→bet := math→mod(data→bet, 5) + 1;
        }
        else {
          if $index_in_set = 8 then {
            code→showPayout(true);
          }
        }
      }
      data→bettext→set_text(data→bet ∥ "");
    }
  }
  data→board→update_on_wall;
  meta private;
}
var discarding : Boolean {
}

action button(text: String, pos: Number) {
  $back := data→board→create_rectangle(90, 190);
  $back→set_color(colors→red);
  $back→set_pos(418, $pos);
  $color := data→board→create_rectangle(60, 170);
  $color→set_color(colors→yellow);
  $color→move_towards($back, 1);
  $lettering := data→board→create_text($color→height, 60, 40, $text);
  $lettering→set_color(colors→red);
  $lettering→move_towards($back, 1);
  $lettering→set_angle(270);
  if $text→equals("DEAL") then {
    data→dealdraw := $lettering;
  }
  $hotspot := data→board→create_rectangle($back→width, $back→height);
  $hotspot→move_towards($back, 1);
  $hotspot→set_color(colors→transparent);
  data→hotspots→add($hotspot);
  meta private;
}
var bettext : Sprite {
}
var banktext : Sprite {
}
var bank : Number {
}
var bet : Number {
}
var dealdraw : Sprite {
}

action score() {
  for 0 ≤ i < 5 do {
    $x := 4 - $i;
    $select := data→cardSprites→at($x * 5 + 3);
    if not $select→is_visible then {
      code→showCard($x, false);
      data→board→update_on_wall;
    }
  }
  for 0 ≤ i1 < 5 do {
    $x1 := 4 - $i1;
    $select1 := data→cardSprites→at($x1 * 5 + 3);
    if not $select1→is_visible then {
      $card1 := code→handDiscard($i1);
      code→update($card1, $x1);
    }
  }
  data→dealdraw→set_text("DEAL");
  data→discarding := false;
  code→handScore;
  data→won := code→classGetPropertyNumber(data→hand, "score") * data→bet;
  if data→won = 1250 then {
    data→won := 4000;
  }
  data→bank := data→bank + data→won;
  bazaar→post_leaderboard_score(data→bank);
  data→banktext→set_text(data→bank ∥ "");
  data→wontext→set_text(data→won ∥ "");
  data→handDisplay→set_text(code→classGetPropertyString(data→hand, "hand"));
  data→handDisplay→show;
  meta private;
}

action handDiscard(index: Number) returns card: Json_Object {
  $cards := code→classGetProperty(data→hand, "cards");
  $card := code→deckDeal;
  $cards := code→arrayEditJson($cards, $index, $card);
  data→hand := code→classSetPropertyJson(data→hand, "cards", $cards);
  meta private;
}

action update(card: Json_Object, i: Number) {
  $suit := code→classGetPropertyString($card, "suit");
  $rank := code→classGetPropertyString($card, "rank");
  data→cardSprites→at($i * 5 + 1)→set_text($rank);
  data→cardSprites→at($i * 5 + 2)→set_text($rank);
  if $suit→equals("H") then {
    data→cardSprites→at($i * 5)→set_picture(data→heart);
    data→cardSprites→at($i * 5 + 1)→set_color(colors→red);
    data→cardSprites→at($i * 5 + 2)→set_color(colors→red);
  }
  else {
    if $suit→equals("C") then {
      data→cardSprites→at($i * 5)→set_picture(data→club);
      data→cardSprites→at($i * 5 + 1)→set_color(colors→black);
      data→cardSprites→at($i * 5 + 2)→set_color(colors→black);
    }
    else {
      if $suit→equals("D") then {
        data→cardSprites→at($i * 5)→set_picture(data→diamond);
        data→cardSprites→at($i * 5 + 1)→set_color(colors→red);
        data→cardSprites→at($i * 5 + 2)→set_color(colors→red);
      }
      else {
        if $suit→equals("S") then {
          data→cardSprites→at($i * 5)→set_picture(data→spade);
          data→cardSprites→at($i * 5 + 1)→set_color(colors→black);
          data→cardSprites→at($i * 5 + 2)→set_color(colors→black);
        }
      }
    }
  }
  code→showCard($i, true);
  data→board→update_on_wall;
  meta private;
}
var back : Picture {
}

action handScore() {
  $cards := code→classGetProperty(data→hand, "cards");
  $ranks := code→newStringCollection();
  $suits := code→newStringCollection();
  $matches := code→newStringCollection();
  for 0 ≤ i < 5 do {
    $s := code→classGetPropertyString($cards→at($i), "suit");
    if not $suits→contains($s) then {
      $suits→add($s);
    }
    $r := code→classGetPropertyString($cards→at($i), "rank");
    if $ranks→contains($r) then {
      if not $matches→contains($r) then {
        $matches→add($r);
      }
    }
    else {
      $ranks→add($r);
    }
  }
  if $ranks→count = 4 then {
    if "JQKA" →contains($matches→at(0)) then {
      code→setScore(1);
    }
    else {
      code→setScore(0);
    }
  }
  else {
    if $ranks→count = 3 then {
      if $matches→count = 1 then {
        code→setScore(3);
      }
      else {
        code→setScore(2);
      }
    }
    else {
      if $ranks→count = 2 then {
        if $matches→count = 1 then {
          code→setScore(7);
        }
        else {
          code→setScore(6);
        }
      }
      else {
        $flush := $suits→count = 1;
        $high := 0;
        $low := 15;
        $A := $ranks→remove("A");
        if $ranks→remove("K") then {
          $high := 13;
        }
        if $ranks→remove("Q") then {
          $high := math→max(12, $high);
        }
        if $ranks→remove("J") then {
          $high := math→max(11, $high);
        }
        foreach n in $ranks where true do {
          $low := math→min($low, $n→to_number);
          $high := math→max($high, $n→to_number);
        }
        if $high = 5 or $low = 10 or ( not $A and $high - $low = 4) then {
          if $flush then {
            if $low = 10 then {
              code→setScore(9);
            }
            else {
              code→setScore(8);
            }
          }
          else {
            code→setScore(4);
          }
        }
        else {
          if $flush then {
            code→setScore(5);
          }
          else {
            code→setScore(0);
          }
        }
      }
    }
  }
  meta private;
}

action setScore(hand: Number) {
  $h := "";
  $s := 0;
  if $hand = 1 then {
    $h := "JACKS OR BETTER";
    $s := 1;
  }
  else {
    if $hand = 2 then {
      $h := "TWO PAIR";
      $s := 2;
    }
    else {
      if $hand = 3 then {
        $h := "THREE OF A KIND";
        $s := 3;
      }
      else {
        if $hand = 4 then {
          $h := "STRAIGHT";
          $s := 4;
        }
        else {
          if $hand = 5 then {
            $h := "FLUSH";
            $s := 6;
          }
          else {
            if $hand = 6 then {
              $h := "FULL HOUSE";
              $s := 9;
            }
            else {
              if $hand = 7 then {
                $h := "FOUR OF A KIND";
                $s := 25;
              }
              else {
                if $hand = 8 then {
                  $h := "STRAIGHT FLUSH";
                  $s := 50;
                }
                else {
                  if $hand = 9 then {
                    $h := "ROYAL FLUSH";
                    $s := 250;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  data→hand := code→classSetProperty(data→hand, "hand", $h);
  data→hand := code→classSetProperty(data→hand, "score", $s ∥ "");
  meta private;
}
var handDisplay : Sprite {
}
var payoutScreen : Sprite_Set {
}

action showPayout(show: Boolean) {
  foreach sprite in data→payoutScreen where true do {
    if $show then {
      $sprite→show;
    }
    else {
      $sprite→hide;
    }
  }
  meta private;
}
var won : Number {
}

action showCard(x: Number, show: Boolean) {
  if $show then {
    data→cardSprites→at($x * 5)→show;
    data→cardSprites→at($x * 5 + 1)→show;
    data→cardSprites→at($x * 5 + 2)→show;
    data→cardSprites→at($x * 5 + 4)→hide;
  }
  else {
    data→cardSprites→at($x * 5)→hide;
    data→cardSprites→at($x * 5 + 1)→hide;
    data→cardSprites→at($x * 5 + 2)→hide;
    data→cardSprites→at($x * 5 + 4)→show;
  }
  meta private;
}

event tap_sprite_in_payoutScreen(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  code→showPayout(false);
  data→board→update_on_wall;
  meta private;
}
var banklabel : Sprite {
}
var wonlabel : Sprite {
}
var wontext : Sprite {
}

event gameloop() {
  if time→now→greater(data→timer) and not data→discarding then {
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
    data→timer := time→now→add_seconds(3);
  }
  meta private;
}
var timer : DateTime {
}

action go() {
  skip;
}

