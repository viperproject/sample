meta version "v2.2,nothing";
meta name "todo item";
meta icon "flowchart";
meta color "#ff008000";
//
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
  }
  resolve core = ♻core with {
  }
}
meta import collection {
  pub "qfhi"
  usage {
  }
  resolve core = ♻core with {
  }
}
meta import instance {
  pub "patj"
  usage {
    action get_number(instance: Json_Object, property: String) returns value: Number
    action get_boolean(instance: Json_Object, property: String) returns value: Boolean
    action get_datetime(instance: Json_Object, property: String) returns value: DateTime
    action get_string(instance: Json_Object, property: String) returns value: String
    action set_boolean(instance: Json_Object, property: String, value: Boolean) returns updated: Json_Object
    action set_number(instance: Json_Object, property: String, value: Number) returns updated: Json_Object
    action set_string(instance: Json_Object, property: String, value: String) returns updated: Json_Object
    action set_datetime(instance: Json_Object, property: String, value: DateTime) returns updated: Json_Object
  }
  resolve core = ♻core with {
  }
  resolve array = ♻array resolve color_helpers = ♻color_helpers with {
  }
  resolve collection = ♻collection with {
  }
}
meta import color_helpers {
  pub "wszv"
  usage {
  }
}
meta import definition {
  pub "sngp"
  usage {
    action init()
    action define_class(name: String)
    action new(class: String) returns instance: Json_Object
    action define_property(class: String, propertyName: String, propertyType: String)
    action is_initialized() returns b: Boolean
    action is_defined_type(type: String) returns b: Boolean
  }
  resolve core = ♻core resolve array = ♻array with {
  }
  resolve collection = ♻collection with {
  }
}

action new(new_id: Number) returns item: Json_Object {
  code→init;
  $item := ♻ definition→new("todo item");
  $item := ♻ instance→set_number($item, "id", $new_id);
  $item := ♻ instance→set_string($item, "name", "New Item");
  $item := ♻ instance→set_datetime($item, "due date", time→now);
}

action init() {
  if not ♻ definition→is_initialized then {
    ♻ definition→init;
  }
  if not ♻ definition→is_defined_type("todo item") then {
    ♻ definition→define_class("todo item");
    ♻ definition→define_property("todo item", "id", "number");
    ♻ definition→define_property("todo item", "name", "string");
    ♻ definition→define_property("todo item", "description", "string");
    ♻ definition→define_property("todo item", "due date", "datetime");
    ♻ definition→define_property("todo item", "complete", "boolean");
  }
}

action to_message(item: Json_Object) returns msg: Message {
  $msg := social→create_message(code→description($item));
  $msg→set_to(code→id($item)→to_string);
  if code→status($item)→equals("Incomplete.") then {
    $msg→set_source(code→status($item) ∥ " Due " ∥ code→due_date($item) ∥ " Do it");
  }
  else {
    $msg→set_source(code→status($item) ∥ " Due");
  }
  $msg→set_time(code→due_date($item));
  $msg→set_from(((code→complete($item)→to_number) + 9744)→to_character ∥ "   " ∥ code→name($item));
}

action name(item: Json_Object) returns s: String {
  $s := ♻ instance→get_string($item, "name");
}

action id(item: Json_Object) returns n: Number {
  $n := ♻ instance→get_number($item, "id");
}

action description(item: Json_Object) returns s: String {
  $s := ♻ instance→get_string($item, "description");
}

action is_defined() returns b: Boolean {
  $b := false;
  if♻ definition→is_initialized then {
    $b := ♻ definition→is_defined_type("todo item");
  }
}

action status(item: Json_Object) returns s: String {
  $s := "Complete.";
  if not code→complete($item) then {
    if time→now→greater_or_equal(code→due_date($item)) then {
      $s := "Overdue.";
    }
    else {
      $s := "Incomplete.";
    }
  }
}

action complete(item: Json_Object) returns b: Boolean {
  $b := ♻ instance→get_boolean($item, "complete");
}

action due_date(item: Json_Object) returns dt: DateTime {
  $dt := ♻ instance→get_datetime($item, "due date");
}

action from_message(msg: Message) returns item: Json_Object {
  $item := ♻ definition→new("todo item");
  $item := ♻ instance→set_number($item, "id", $msg→to→to_number);
  $item := ♻ instance→set_string($item, "name", $msg→from→replace_regex("[☐|☑]\\s+", ""));
  $item := ♻ instance→set_string($item, "description", $msg→message);
  $item := ♻ instance→set_boolean($item, "complete", $msg→from→at(0)→equals("☑"));
  $item := ♻ instance→set_datetime($item, "due date", $msg→time);
}

action set_complete(item: Json_Object, b: Boolean) returns updated: Json_Object {
  $updated := ♻ instance→set_boolean($item, "complete", $b);
}

action set_description(item: Json_Object, s: String) returns updated: Json_Object {
  $updated := ♻ instance→set_string($item, "description", $s);
}

action set_due_date(item: Json_Object, dt: DateTime) returns updated: Json_Object {
  $updated := ♻ instance→set_datetime($item, "due date", $dt);
}

action set_name(item: Json_Object, s: String) returns updated: Json_Object {
  $updated := ♻ instance→set_string($item, "name", $s);
}

action toggle(item: Json_Object) returns updated: Json_Object {
  $updated := ♻ instance→set_boolean($item, "complete", not code→complete($item));
}

