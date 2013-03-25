meta version "v2.2,nothing";
meta name "playing card";
meta icon "flowchart";
meta color "#ff008000";
// Reusable library for working with playing cards.
meta isLibrary "yes";
meta platform "current";
meta import suits {
  pub "qzig"
  usage {
    action all() returns s: String
  }
}
meta import definition {
  pub "txih"
  usage {
    action define_class(name: String)
    action new(class: String) returns instance: Json_Object
    action define_property(class: String, propertyName: String, propertyType: String)
  }
  resolve core = ♻core resolve array = ♻array with {
  }
  resolve collection = ♻collection with {
  }
}
meta import core {
  pub "dnoo"
  usage {
  }
}
meta import array {
  pub "bzjc"
  usage {
  }
  resolve core = ♻core with {
  }
}
meta import collection {
  pub "tizp"
  usage {
  }
  resolve core = ♻core with {
  }
}
meta import instance {
  pub "sbog"
  usage {
    action get_number(instance: Json_Object, property: String) returns value: Number
    action get_string(instance: Json_Object, property: String) returns value: String
    action get_color(instance: Json_Object, property: String) returns value: Color
    action set_number(instance: Json_Object, property: String, value: Number) returns updated: Json_Object
    action set_color(instance: Json_Object, property: String, value: Color) returns updated: Json_Object
    action set_string(instance: Json_Object, property: String, value: String) returns updated: Json_Object
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

action new(value: Number) returns c: Json_Object {
  $c := ♻ definition→new("card");
  $c := ♻ instance→set_number($c, "value", $value);
  $suit := math→mod($value, 4);
  $rank := math→mod($value, 13);
  $c := ♻ instance→set_string($c, "suit", (♻ suits→all)→at($suit));
  $clr := colors→red;
  if math→mod($suit, 2) = 1 then {
    $clr := colors→black;
  }
  $c := ♻ instance→set_color($c, "color", $clr);
  if $rank = 12 then {
    $c := ♻ instance→set_string($c, "rank", 10→to_string);
  }
  else {
    $c := ♻ instance→set_string($c, "rank", "A23456789JQK" →at($rank));
  }
}

action equals(c: Json_Object, other: Json_Object) returns e: Boolean {
  $e := code→value($c) = code→value($other);
}

action init() {
  ♻ definition→define_class("card");
  ♻ definition→define_property("card", "value", "number");
  ♻ definition→define_property("card", "suit", "string");
  ♻ definition→define_property("card", "rank", "string");
  ♻ definition→define_property("card", "color", "color");
}

action to_string(c: Json_Object) returns s: String {
  $s := code→rank($c) ∥ code→suit($c);
}

action rank(c: Json_Object) returns s: String {
  $s := ♻ instance→get_string($c, "rank");
}

action value(c: Json_Object) returns n: Number {
  $n := ♻ instance→get_number($c, "value");
}

action color(c: Json_Object) returns clr: Color {
  $clr := ♻ instance→get_color($c, "color");
}

action suit(c: Json_Object) returns s: String {
  $s := ♻ instance→get_string($c, "suit");
}

