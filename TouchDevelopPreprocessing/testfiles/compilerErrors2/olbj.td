meta version "v2.2,nothing";
meta name "SupragMobileSuite";
//
meta platform "current";

action zJsonserializeString(value: String) returns s: String {
  $strings := code→newStringCollection;
  $strings→add("\"");
  foreach char in $value where true do {
    $x := $char→to_unicode;
    if $x < 32 or $x = 34 or $x = 47 or $x = 94 then {
      $strings→add("\\u" ∥ code→zJsonToHex($x));
    }
    else {
      $strings→add($char);
    }
  }
  $strings→add("\"");
  $s := $strings→join("");
  meta private;
}

action SerialChecker() {
  //
  // Initialize
  data→Controls := code→zJsonnewObject();
  data→BG := media→create_full_board;
  data→BG→post_to_wall;
  data→BGColor := colors→black;
  data→Sprites := data→BG→create_sprite_set;
  //
  code→Load;
  data→Running := true;
  code→DrawControls;
  while data→Running do {
    code→CheckTouch;
    data→BG→update_on_wall;
  }
}

action newStringCollection() returns strings: String_Collection {
  $strings := "" →split(",");
  $strings→clear;
  meta private;
}

action zJsonToHex(x: Number) returns s: String {
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

action zJsonnewObject() returns json: Json_Object {
  $json := web→json("{}");
  meta private;
}
var Controls : Json_Object {
}
var BG : Board {
}
var BGColor : Color {
}
var Running : Boolean {
}

action Load() {
  code→addButton("Gjbcf", true, "Exit", 14, 200, 421, 200, 50);
  code→addButton("Txt", true, "Drücken Sie hier,\num eine Seriennr. zu prüfen.", 20, 10, 100, 450, 300);
  //
  // Test
  data→Controls := code→zJsonobjectSetSubValue(data→Controls, "Enabled", (true ∥ ""), "Zweites");
  meta private;
}

action DrawControls() {
  //
  // Reset Display
  foreach sprite2 in data→Sprites where true do {
    data→Sprites→remove($sprite2);
  }
  $black := data→BG→create_rectangle(500, 1000);
  $black→set_color(data→BGColor);
  //
  // Draw Controls
  foreach json in data→Controls→keys where data→Controls→field($json)→field("Enabled")→to_boolean do {
    $posX := data→Controls→field($json)→field("PosX")→to_number;
    $posY := data→Controls→field($json)→field("PosY")→to_number;
    $width := data→Controls→field($json)→field("Width")→to_number;
    $height := data→Controls→field($json)→field("Height")→to_number;
    $type := data→Controls→field($json)→field("Type")→to_string;
    //
    // Types
    if $type→equals("Label") then {
      $text := data→Controls→field($json)→field("Text")→to_string;
      $fontSize := data→Controls→field($json)→field("FontSize")→to_number;
      $lines := $text→split("\n");
      $sprite := data→BG→create_text($width, $lines→count * $fontSize * 1.5, $fontSize, $text);
      data→Sprites→add($sprite);
      $sprite→set_pos($posX + ($width / 2), $posY + ($height / 2));
    }
    else {
      //
      if $type→equals("Button") then {
        $text1 := data→Controls→field($json)→field("Text")→to_string;
        $fontSize1 := data→Controls→field($json)→field("FontSize")→to_number;
        $shadow := data→BG→create_rectangle($width, $height);
        data→Sprites→add($shadow);
        $bg := data→BG→create_rectangle($width, $height);
        data→Sprites→add($bg);
        $lines1 := $text1→split("\n");
        $sprite1 := data→BG→create_text($width, $lines1→count * $fontSize1 * 1.5, $fontSize1, $text1);
        data→Sprites→add($sprite1);
        $shadow→set_pos($posX + ($width / 2) + 2, $posY + ($height / 2) + 2);
        $bg→set_pos($posX + ($width / 2), $posY + ($height / 2));
        $sprite1→set_pos($posX + ($width / 2), $posY + ($height / 2));
        $bg→set_color(colors→green);
        $sprite1→set_color(colors→black);
      }
    }
  }
  meta private;
}

action CheckTouch() {
  //
  // Touched Point
  $t := math→create_vector3(0, 0, 0);
  if data→BG→touched then {
    $t := data→BG→touch_current;
  }
  $x := 0;
  foreach json in data→Controls→keys where true do {
    // Control Values
    $posX := data→Controls→field($json)→field("PosX")→to_number;
    $posY := data→Controls→field($json)→field("PosY")→to_number;
    $width := data→Controls→field($json)→field("Width")→to_number;
    $height := data→Controls→field($json)→field("Height")→to_number;
    //
    // CheckTouchOver
    if $posX < $t→x and $posX + $width > $t→x and $posY < $t→y and $posY + $height > $t→y then {
      $touched := data→Controls→field($json)→field("Touched")→to_boolean;
      $type := data→Controls→field($json)→field("Type")→to_string;
      if $touched→equals(false) then {
        data→Controls := code→zJsonobjectSetSubValue(data→Controls, "Touched", (true ∥ ""), $json);
        if $type→equals("Button") then {
          $x := $x + 1;
          data→Sprites→at($x)→set_pos(data→Sprites→at($x)→x + 2, data→Sprites→at($x)→y + 2);
          $x := $x + 1;
          data→Sprites→at($x)→set_pos(data→Sprites→at($x)→x + 2, data→Sprites→at($x)→y + 2);
        }
        else {
          data→Sprites→at($x)→set_pos(data→Sprites→at($x)→x + 2, data→Sprites→at($x)→y + 2);
        }
      }
      else {
        if $type→equals("Button") then {
          $x := $x + 2;
        }
      }
    }
    else {
      $touched1 := data→Controls→field($json)→field("Touched")→to_boolean;
      $type1 := data→Controls→field($json)→field("Type")→to_string;
      if $touched1 then {
        data→Controls := code→zJsonobjectSetSubValue(data→Controls, "Touched", (false ∥ ""), $json);
        if $type1→equals("Button") then {
          $x := $x + 1;
          data→Sprites→at($x)→set_pos(data→Sprites→at($x)→x - 2, data→Sprites→at($x)→y - 2);
          $x := $x + 1;
          data→Sprites→at($x)→set_pos(data→Sprites→at($x)→x - 2, data→Sprites→at($x)→y - 2);
          $id := data→Controls→field($json)→field("Id")→to_number;
          code→ButtonPressed($id);
          ... ;
        }
        else {
          data→Sprites→at($x)→set_pos(data→Sprites→at($x)→x - 2, data→Sprites→at($x)→y - 2);
        }
      }
      else {
        if $type1→equals("Button") then {
          $x := $x + 2;
        }
      }
    }
    $x := $x + 1;
  }
  meta private;
}
var Sprites : Sprite_Set {
}

action addControl(jsonInn: Json_Object, enabled: Boolean, posX: Number, posY: Number, width: Number, height: Number) returns json: Json_Object {
  $json := code→zJsonobjectAddValue($jsonInn, "Id", data→Controls→keys→count ∥ "");
  $json := code→zJsonobjectAddValue($json, "Enabled", $enabled ∥ "");
  $json := code→zJsonobjectAddValue($json, "PosX", $posX ∥ "");
  $json := code→zJsonobjectAddValue($json, "PosY", $posY ∥ "");
  $json := code→zJsonobjectAddValue($json, "Width", $width ∥ "");
  $json := code→zJsonobjectAddValue($json, "Height", $height ∥ "");
  $json := code→zJsonobjectAddValue($json, "Touched", false ∥ "");
  meta private;
}

action addLabel(name: String, enabled: Boolean, text: String, fontSize: Number, posX: Number, posY: Number, width: Number, height: Number) {
  $json := code→zJsonnewObject();
  $json := code→zJsonobjectAddString($json, "Type", "Label");
  $json := code→addControl($json, $enabled, $posX, $posY, $width, $height);
  $json := code→zJsonobjectAddString($json, "Text", $text);
  $json := code→zJsonobjectAddValue($json, "FontSize", $fontSize ∥ "");
  data→Controls := code→zJsonobjectAddJson(data→Controls, $name, $json);
  meta private;
}

action addButton(name: String, enabled: Boolean, text: String, fontSize: Number, posX: Number, posY: Number, width: Number, height: Number) {
  $json := code→zJsonnewObject();
  $json := code→zJsonobjectAddString($json, "Type", "Button");
  $json := code→addControl($json, $enabled, $posX, $posY, $width, $height);
  $json := code→zJsonobjectAddString($json, "Text", $text);
  $json := code→zJsonobjectAddValue($json, "FontSize", $fontSize ∥ "");
  data→Controls := code→zJsonobjectAddJson(data→Controls, $name, $json);
  meta private;
}

action zJsonobjectSetSubValue(@in: Json_Object, Name: String, newValue: String, path: String) returns out: Json_Object {
  $paths := $path→split(".");
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    $v := code→zJsonserializeJson($@in→field($n));
    if $n→equals($paths→at(0)) then {
      // Subitem
      if $paths→count = 1 then {
        // Letztes subitem
        $json := code→zJsonobjectSetValue($@in→field($n), $Name, $newValue);
        $object→add($n ∥ ":" ∥ code→zJsonserializeJson($json));
      }
      else {
        // Subitems
        $paths→remove_at(0);
        $json1 := code→zJsonobjectSetSubValue($@in→field($n), $paths→at(0), $newValue, $paths ∥ "");
        $object→add($n ∥ ":" ∥ code→zJsonserializeJson($json1));
      }
    }
    else {
      // Normales Feld
      $object→add($n ∥ ":" ∥ $v);
    }
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action zJsonobjectSetValue(@in: Json_Object, Name: String, newValue: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    $v := code→zJsonserializeJson($@in→field($n));
    if $n→equals($Name) then {
      $object→add($n ∥ ":" ∥ $newValue);
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

action zJsonobjectAddString(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $out := code→zJsonobjectAddValue($@in, $name, code→zJsonserializeString($value));
  meta private;
}

action zJsonobjectAddValue(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $object→add($@in→keys→at($k) ∥ ":" ∥ code→zJsonserializeJson($@in→field($@in→keys→at($k))));
  }
  $object→add($name ∥ ":" ∥ $value);
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action zJsonobjectAddJson(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object {
  $out := code→zJsonobjectAddValue($@in, $name, code→zJsonserializeJson($value));
  meta private;
}

action ButtonPressed(id: Number) {
  if data→BG→touched→equals(false) then {
    if $id = 1 then {
      $s := wall→ask_string("");
      wall→clear;
      ... ;
      data→BG→post_to_wall;
      $s1 := web→download("Http://suprag.dynalias.net:81/GarantieChecker.spg?RMACheckSerial=" ∥ $s ∥ "&RMAUID=123456&RMAVorname=nichts&RMAName=niemand&RMAFirma=intern&RMASessionID=123456&RMACheckSerialMobile=yes");
      $s1 := $s1→replace("<br>", "\\r\\n");
      $s1 := $s1→replace("<b>", "");
      $s1 := $s1→replace("</b>", "");
      data→Controls := code→zJsonobjectSetSubValue(data→Controls, "Text", code→zJsonserializeString($s1), "Txt");
      code→DrawControls;
    }
    else {
      if $id = 0 then {
        data→Running := false;
      }
    }
  }
  meta private;
}

action zJsonserializeJson(json: Json_Object) returns s: String {
  $s := "null";
  if not $json→is_invalid then {
    if $json→kind→equals("string") then {
      $s := code→zJsonserializeString($json→to_string);
    }
    else {
      if $json→kind→equals("object") then {
        $s := code→zJsonserializeObject($json);
      }
      else {
        if $json→kind→equals("array") then {
          $s := code→zJsonserializeArray($json);
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

action zJsonserializeObject(json: Json_Object) returns s: String {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $json→keys→count do {
    $object→add($json→keys→at($k) ∥ ":" ∥ code→zJsonserializeJson($json→field($json→keys→at($k))));
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $s := $strings→join("");
  meta private;
}

action zJsonserializeArray(json: Json_Object) returns s: String {
  $strings := code→newStringCollection;
  $strings→add("[");
  $array := code→newStringCollection;
  for 0 ≤ i < $json→count do {
    $array→add(code→zJsonserializeJson($json→at($i)));
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $s := $strings→join("");
  meta private;
}

