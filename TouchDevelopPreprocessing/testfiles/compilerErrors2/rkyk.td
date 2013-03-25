meta version "v2.2,nothing";
meta name "jsonlib";
//
meta platform "current";

action main() {
  "This script can be used as the basis for any script that needs to edit json. If you improve it, post a new version." →post_to_wall;
  time→sleep(3);
  code→testJson;
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

action testSerialize(test_json: String) {
  if $test_json→is_empty then {
    $test_json := "{test1:[1,true,\"Hello World!\"],test2:\"test\"}";
  }
  "Validating json..." →post_to_wall;
  $json := web→json($test_json);
  "Valid. Serializing..." →post_to_wall;
  $dt := time→now;
  code→serializeJson($json)→post_to_wall;
  ("Complete. Serialization took:  " ∥ time→now→subtract($dt) ∥ " seconds.")→post_to_wall;
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

action newStringCollection() returns strings: String_Collection {
  $strings := "" →split(",");
  $strings→clear;
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

action arrayInsertString(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $out := code→arrayInsertValue($@in, $at, code→serializeString($value));
  meta private;
}

action arrayInsertJson(@in: Json_Object, at: Number, value: Json_Object) returns out: Json_Object {
  $out := code→arrayInsertValue($@in, $at, code→serializeJson($value));
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

action testJson() {
  $commands := "Add/Insert\nRemove\nNew Object\nNew Array" →split("\n");
  $json := code→newArray();
  while true do {
    wall→clear;
    $json→post_to_wall;
    if $json→kind→equals("array") then {
      ("Count:  " ∥ $json→count)→post_to_wall;
    }
    else {
      ("Count:  " ∥ $json→keys→count)→post_to_wall;
    }
    $x := wall→pick_string("What do you want to do?", "Choose Action", $commands);
    wall→clear;
    if $x = 0 then {
      if $json→kind→equals("array") then {
        $s2 := "Insert At ";
        $i := wall→ask_number($s2);
        $s2 := $s2 ∥ $i;
        $json := code→arrayInsertValue($json, $i, code→testCreate($s2));
      }
      else {
        $s3 := "Add Named Object ";
        $s := wall→ask_string($s3);
        $s3 := $s3 ∥ $s;
        $json := code→objectAddValue($json, $s, code→testCreate($s3));
      }
    }
    else {
      if $x = 1 then {
        if $json→kind→equals("array") then {
          $i1 := wall→ask_number("Remove At");
          $json := code→arrayRemoveAt($json, $i1);
        }
        else {
          $s1 := wall→ask_string("Remove Named Object");
          $json := code→objectRemoveByName($json, $s1);
        }
      }
      else {
        if $x = 2 then {
          $json := code→newObject;
        }
        else {
          if $x = 3 then {
            $json := code→newArray;
          }
        }
      }
    }
  }
}

action newArray() returns json: Json_Object {
  $json := web→json("[]");
  meta private;
}

action newObject() returns json: Json_Object {
  $json := web→json("{}");
  meta private;
}

action testCreate(text: String) returns s: String {
  wall→clear;
  $t := wall→pick_string($text, "Which json kind?", "string\nnumber\nboolean\nobject\narray" →split("\n"));
  wall→clear;
  if $t = 0 then {
    $s := code→serializeString(wall→ask_string("Enter String"));
  }
  else {
    if $t = 1 then {
      $s := wall→ask_number("Enter Number") ∥ "";
    }
    else {
      if $t = 2 then {
        $s := wall→ask_boolean("Enter Boolean", "") ∥ "";
      }
      else {
        if $t = 3 then {
          $s := "{test:\"data\"}";
        }
        else {
          $s := "[1,2,3]";
        }
      }
    }
  }
  meta private;
}

