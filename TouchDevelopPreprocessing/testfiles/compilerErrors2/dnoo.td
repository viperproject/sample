meta version "v2.2,nothing";
meta name "json core";
meta icon "flowchart";
meta color "#ff008000";
// Core json actions used for serialization.
meta isLibrary "yes";
meta platform "current";

action to_hex(x: Number) returns s: String {
  $strings := collections→create_string_collection;
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

action serialize_string(value: String) returns s: String {
  $strings := collections→create_string_collection;
  $strings→add("\"");
  foreach char in $value where true do {
    $x := $char→to_unicode;
    if $x < 32 or $x = 34 or $x = 47 or $x = 94 then {
      $strings→add("\\u" ∥ code→to_hex($x));
    }
    else {
      $strings→add($char);
    }
  }
  $strings→add("\"");
  $s := $strings→join("");
}

action serialize_json(json: Json_Object) returns s: String {
  $s := "null";
  if not $json→is_invalid then {
    if $json→kind→equals("string") then {
      $s := code→serialize_string($json→to_string);
    }
    else {
      if $json→kind→equals("object") then {
        $s := code→serialize_object($json);
      }
      else {
        if $json→kind→equals("array") then {
          $s := code→serialize_array($json);
        }
        else {
          if $json→kind→equals("number") then {
            $s := $json→to_number→to_string;
          }
          else {
            $s := $json→to_boolean→to_string;
          }
        }
      }
    }
  }
}

action serialize_array(json: Json_Object) returns s: String {
  $strings := collections→create_string_collection;
  $strings→add("[");
  $array := collections→create_string_collection;
  for 0 ≤ i < $json→count do {
    $array→add(code→serialize_json($json→at($i)));
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $s := $strings→join("");
  meta private;
}

action serialize_object(json: Json_Object) returns s: String {
  $strings := collections→create_string_collection;
  $strings→add("{");
  $object := collections→create_string_collection;
  for 0 ≤ k < $json→keys→count do {
    $object→add($json→keys→at($k) ∥ ":" ∥ code→serialize_json($json→field($json→keys→at($k))));
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $s := $strings→join("");
  meta private;
}

action from_strings(strings: String_Collection) returns json: Json_Object {
  $json := code→new($strings→join(""));
}

action new(s: String) returns json: Json_Object {
  $json := web→json($s);
}

