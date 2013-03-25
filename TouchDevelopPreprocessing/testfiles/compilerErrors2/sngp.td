meta version "v2.2,nothing";
meta name "data definition";
meta icon "flowchart";
meta color "#ff008000";
// Reusable library for defining the data structures of complex objects.
meta isLibrary "yes";
meta platform "current";
meta import core {
  pub "jtfn"
  usage {
  }
}
meta import array {
  pub "txuv"
  usage {
    action new() returns json: Json_Object
    action add_json(@in: Json_Object, value: Json_Object) returns out: Json_Object
    action add_string(@in: Json_Object, value: String) returns out: Json_Object
    action edit_json(@in: Json_Object, at: Number, value: Json_Object) returns out: Json_Object
  }
  resolve core = ♻core with {
  }
}
meta import collection {
  pub "qfhi"
  usage {
    action new() returns json: Json_Object
    action edit_json(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object
    action add_value(@in: Json_Object, name: String, value: String) returns out: Json_Object
    action add_string(@in: Json_Object, name: String, value: String) returns out: Json_Object
    action add_json(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object
  }
  resolve core = ♻core with {
  }
}
var classData : Json_Object {
}

action definitions() returns json: Json_Object {
  $json := data→classData;
}

action init() {
  data→classData := ♻ collection→new;
  data→classData := ♻ collection→add_string(data→classData, "type", "class data");
  data→classData := ♻ collection→add_string(data→classData, "version", "1.0");
  data→classData := ♻ collection→add_json(data→classData, "definitions", ♻ array→new());
}

action get_def(class: String) returns def: Json_Object {
  $classes := data→classData→field("definitions");
  $def := ♻ collection→new;
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

action add_def(class: Json_Object) {
  data→classData := ♻ collection→edit_json(data→classData, "definitions", ♻ array→add_json(data→classData→field("definitions"), $class));
  meta private;
}

action set_def(class: Json_Object) {
  $classes := data→classData→field("definitions");
  $name := $class→string("name");
  $c := 0;
  while $c < $classes→count do {
    if $classes→at($c)→string("name")→equals($name) then {
      data→classData := ♻ collection→edit_json(data→classData, "definitions", ♻ array→edit_json($classes, $c, $class));
      $c := 99;
    }
    $c := $c + 1;
  }
  meta private;
}

action define_inherited_class(name: String, inherits: String) {
  $class := ♻ collection→new;
  $class := ♻ collection→add_string($class, "type", "class");
  $class := ♻ collection→add_string($class, "name", $name);
  $class := ♻ collection→add_string($class, "inherits", $inherits);
  $props := ♻ array→new;
  if not $inherits→is_empty then {
    $inherited := code→get_def($inherits);
    $props := $inherited→field("properties");
  }
  $class := ♻ collection→add_json($class, "properties", $props);
  code→add_def($class);
}

action define_class(name: String) {
  code→define_inherited_class($name, "");
}

action definition(class: String) returns def: Json_Object {
  $def := code→get_def($class);
}

action new(class: String) returns instance: Json_Object {
  $instance := ♻ collection→new;
  $instance := ♻ collection→add_string($instance, "type", $class);
  $inherits := ♻ array→new;
  $def := code→get_def($class);
  $defInherits := $def→string("inherits");
  while not $defInherits→is_empty do {
    $inherits := ♻ array→add_string($inherits, $defInherits);
    $defInherits := code→get_def($defInherits)→string("inherits");
  }
  $instance := ♻ collection→add_json($instance, "inherits", $inherits);
  $props := $def→field("properties");
  for 0 ≤ i < $props→count do {
    $p := $props→at($i);
    $type := $p→string("type");
    $name := $p→string("name");
    if $type→equals("string") or $type→equals("datetime") then {
      $instance := ♻ collection→add_string($instance, $name, "");
    }
    else {
      if $type→equals("number") or $type→equals("color") then {
        $instance := ♻ collection→add_value($instance, $name, 0→to_string);
      }
      else {
        if $type→equals("boolean") then {
          $instance := ♻ collection→add_value($instance, $name, false→to_string);
        }
        else {
          $instance := ♻ collection→add_value($instance, $name, "null");
        }
      }
    }
  }
}

action define_property(class: String, propertyName: String, propertyType: String) {
  $prop := ♻ collection→new;
  $prop := ♻ collection→add_string($prop, "type", $propertyType);
  $prop := ♻ collection→add_string($prop, "name", $propertyName);
  $def := code→get_def($class);
  $props := ♻ array→add_json($def→field("properties"), $prop);
  $def := ♻ collection→edit_json($def, "properties", $props);
  code→set_def($def);
}

action is_initialized() returns b: Boolean {
  $b := not data→classData→is_invalid;
}

action is_defined_type(type: String) returns b: Boolean {
  $b := code→get_def($type)→keys→count > 0;
}

