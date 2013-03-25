meta version "v2.2,nothing";
meta name "data instance";
meta icon "flowchart";
meta color "#ff008000";
// Reusable library for working with instances of complex objects.
meta isLibrary "yes";
meta platform "current";
meta import core {
  pub "dnoo"
  usage {
    action serialize_string(value: String) returns s: String
    action serialize_json(json: Json_Object) returns s: String
  }
}
meta import array {
  pub "bzjc"
  usage {
  }
  resolve core = ♻core with {
  }
}
meta import color_helpers {
  pub "wszv"
  usage {
    action color_to_number(c: Color) returns res: Number
  }
}
meta import collection {
  pub "tizp"
  usage {
    action edit_value(@in: Json_Object, name: String, value: String) returns out: Json_Object
  }
  resolve core = ♻core with {
  }
}

action get_json(instance: Json_Object, property: String) returns value: Json_Object {
  $value := $instance→field($property);
}

action is_type(instance: Json_Object, type: String) returns b: Boolean {
  $b := $instance→string("type")→equals($type);
  if not $b then {
    $inherits := $instance→field("inherits");
    $i := 0;
    while $i < $inherits→count and not $b do {
      $b := $inherits→at($i)→to_string→equals($type);
      $i := $i + 1;
    }
  }
}

action get_number(instance: Json_Object, property: String) returns value: Number {
  $value := code→get_json($instance, $property)→to_number;
}

action get_boolean(instance: Json_Object, property: String) returns value: Boolean {
  $value := code→get_json($instance, $property)→to_boolean;
}

action get_datetime(instance: Json_Object, property: String) returns value: DateTime {
  $value := code→get_json($instance, $property)→to_time;
}

action get_string(instance: Json_Object, property: String) returns value: String {
  $value := code→get_json($instance, $property)→to_string;
}

action set_property(instance: Json_Object, property: String, value: String) returns updated: Json_Object {
  $updated := ♻ collection→edit_value($instance, $property, $value);
  meta private;
}

action get_type(json: Json_Object) returns type: String {
  $type := $json→string("type");
}

action get_color(instance: Json_Object, property: String) returns value: Color {
  $value := code→get_json($instance, $property)→to_number→to_color;
}

action set_boolean(instance: Json_Object, property: String, value: Boolean) returns updated: Json_Object {
  $updated := code→set_property($instance, $property, $value→to_string);
}

action set_number(instance: Json_Object, property: String, value: Number) returns updated: Json_Object {
  $updated := code→set_property($instance, $property, $value→to_string);
}

action set_color(instance: Json_Object, property: String, value: Color) returns updated: Json_Object {
  $updated := code→set_number($instance, $property, ♻ color_helpers→color_to_number($value));
}

action set_string(instance: Json_Object, property: String, value: String) returns updated: Json_Object {
  $updated := code→set_property($instance, $property, ♻ core→serialize_string($value));
}

action set_datetime(instance: Json_Object, property: String, value: DateTime) returns updated: Json_Object {
  $updated := code→set_string($instance, $property, $value→to_string);
}

action set_json(instance: Json_Object, property: String, value: Json_Object) returns updated: Json_Object {
  $updated := code→set_property($instance, $property, ♻ core→serialize_json($value));
}

