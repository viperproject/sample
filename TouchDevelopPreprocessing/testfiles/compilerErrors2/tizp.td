meta version "v2.2,nothing";
meta name "json collection";
meta icon "flowchart";
meta color "#ff008000";
// Reusable library for creating and editing json collections.
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
  $json := ♻ core→new("{}");
}

action from_strings(strings: String_Collection) returns json: Json_Object {
  $outer := collections→create_string_collection;
  $outer→add("{");
  $outer→add($strings→join(","));
  $outer→add("}");
  $json := ♻ core→from_strings($outer);
}

action rename_item(@in: Json_Object, oldName: String, newName: String) returns out: Json_Object {
  $object := collections→create_string_collection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    $v := ♻ core→serialize_json($@in→field($n));
    if $n→equals($oldName) then {
      $object→add($newName ∥ ":" ∥ $v);
    }
    else {
      $object→add($n ∥ ":" ∥ $v);
    }
  }
  $out := code→from_strings($object);
}

action remove_item(@in: Json_Object, name: String) returns out: Json_Object {
  $object := collections→create_string_collection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    if not $n→equals($name) then {
      $object→add($n ∥ ":" ∥ ♻ core→serialize_json($@in→field($@in→keys→at($k))));
    }
  }
  $out := code→from_strings($object);
}

action edit_value(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $object := collections→create_string_collection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    if $n→equals($name) then {
      $object→add($n ∥ ":" ∥ $value);
    }
    else {
      $object→add($n ∥ ":" ∥ ♻ core→serialize_json($@in→field($n)));
    }
  }
  $out := code→from_strings($object);
}

action edit_string(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $out := code→edit_value($@in, $name, ♻ core→serialize_string($value));
}

action edit_json(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object {
  $out := code→edit_value($@in, $name, ♻ core→serialize_json($value));
}

action add_value(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $object := collections→create_string_collection;
  for 0 ≤ k < $@in→keys→count do {
    $object→add($@in→keys→at($k) ∥ ":" ∥ ♻ core→serialize_json($@in→field($@in→keys→at($k))));
  }
  $object→add($name ∥ ":" ∥ $value);
  $out := code→from_strings($object);
}

action add_string(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $out := code→add_value($@in, $name, ♻ core→serialize_string($value));
}

action add_json(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object {
  $out := code→add_value($@in, $name, ♻ core→serialize_json($value));
}

