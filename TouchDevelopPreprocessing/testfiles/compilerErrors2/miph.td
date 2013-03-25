meta version "v2.2,nothing";
meta name "json array test6";
meta icon "flowchart";
meta color "#ff008000";
// Reusable library for editing json arrays.
meta isLibrary "yes";
meta platform "current";
meta import core {
  pub "dnoo"
  usage {
    action serialize_string(value: String) returns s: String
    action serialize_json(json: Json_Object) returns s: String
    action from_strings(strings: String_Collection) returns json: Json_Object
    action new(s: String) returns json: Json_Object
  }
}

action new() returns json: Json_Object {
  $json := ♻ core→new("[]");
}

action add_value(@in: Json_Object, value: String) returns out: Json_Object {
  $out := code→insert_value($@in, $@in→count, $value);
}

action insert_value(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $array := collections→create_string_collection;
  for 0 ≤ i < $@in→count do {
    if $i = $at then {
      $array→add($value);
    }
    $array→add(♻ core→serialize_json($@in→at($i)));
  }
  if $at = $@in→count then {
    $array→add($value);
  }
  $out := code→from_strings($array);
}

action edit_value(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $array := collections→create_string_collection;
  for 0 ≤ i < $@in→count do {
    if $i = $at then {
      $array→add($value);
    }
    else {
      $array→add(♻ core→serialize_json($@in→at($i)));
    }
  }
  $out := code→from_strings($array);
}

action remove_at(@in: Json_Object, at: Number) returns out: Json_Object {
  $array := collections→create_string_collection;
  for 0 ≤ i < $@in→count do {
    if $i ≠ $at then {
      $array→add(♻ core→serialize_json($@in→at($i)));
    }
  }
  $out := code→from_strings($array);
}

action from_strings(strings: String_Collection) returns json: Json_Object {
  $outer := collections→create_string_collection;
  $outer→add("[");
  $outer→add($strings→join(","));
  $outer→add("]");
  $json := ♻ core→from_strings($outer);
}

action add_json(@in: Json_Object, value: Json_Object) returns out: Json_Object {
  $out := code→add_value($@in, ♻ core→serialize_json($value));
}

action add_string(@in: Json_Object, value: String) returns out: Json_Object {
  $out := code→add_value($@in, ♻ core→serialize_string($value));
}

action edit_json(@in: Json_Object, at: Number, value: Json_Object) returns out: Json_Object {
  $out := code→edit_value($@in, $at, ♻ core→serialize_json($value));
}

action edit_string(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $out := code→edit_value($@in, $at, ♻ core→serialize_string($value));
}

action insert_json(@in: Json_Object, at: Number, value: Json_Object) returns out: Json_Object {
  $out := code→insert_value($@in, $at, ♻ core→serialize_json($value));
}

action insert_string(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $out := code→insert_value($@in, $at, ♻ core→serialize_string($value));
}

