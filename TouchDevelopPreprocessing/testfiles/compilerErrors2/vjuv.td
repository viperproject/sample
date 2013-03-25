meta version "v2.2,nothing";
meta name "todo item collection";
meta icon "flowchart";
meta color "#ff008000";
// Reusable library for working with a collection of todo items.
meta isLibrary "yes";
meta platform "current";
meta import todo_item {
  pub "cpfp"
  usage {
    action new(new_id: Number) returns item: Json_Object
    action init()
    action to_message(item: Json_Object) returns msg: Message
    action id(item: Json_Object) returns n: Number
    action is_defined() returns b: Boolean
    action toggle(item: Json_Object) returns updated: Json_Object
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection resolve instance = ♻instance with {
  }
  resolve color_helpers = ♻color_helpers resolve definition = ♻definition with {
  }
}
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
    action new() returns json: Json_Object
    action remove_item(@in: Json_Object, name: String) returns out: Json_Object
    action edit_json(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object
    action add_json(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object
  }
  resolve core = ♻core with {
  }
}
meta import color_helpers {
  pub "wszv"
  usage {
  }
}
meta import instance {
  pub "patj"
  usage {
  }
  resolve core = ♻core with {
  }
  resolve array = ♻array resolve color_helpers = ♻color_helpers with {
  }
  resolve collection = ♻collection with {
  }
}
meta import definition {
  pub "sngp"
  usage {
  }
  resolve core = ♻core resolve array = ♻array with {
  }
  resolve collection = ♻collection with {
  }
}

action new() {
  data→list := ♻ collection→new;
  if not ♻ todo_item→is_defined then {
    ♻ todo_item→init;
  }
}
var list : Json_Object {
}

action add() returns id: Number {
  $item := ♻ todo_item→new(time→now→subtract(time→create(2000, 1, 1, 12, 0, 0)));
  $id := ♻ todo_item→id($item);
  data→list := ♻ collection→add_json(data→list, $id→to_string, $item);
}

action get_by_id(id: Number) returns item: Json_Object {
  $item := data→list→field($id→to_string);
}

action to_messages() returns msgs: Message_Collection {
  $msgs := collections→create_message_collection;
  foreach key in data→list→keys where true do {
    $msgs→add(♻ todo_item→to_message(data→list→field($key)));
  }
  $msgs→sort_by_date;
}

action toggle(id: Number) {
  code→save($id, ♻ todo_item→toggle(code→get_by_id($id)));
}

action save(id: Number, item: Json_Object) {
  data→list := ♻ collection→edit_json(data→list, $id→to_string, $item);
  meta private;
}

action delete(id: Number) {
  data→list := ♻ collection→remove_item(data→list, $id→to_string);
}

action edit(item: Json_Object) {
  code→save(♻ todo_item→id($item), $item);
}

action is_invalid() returns b: Boolean {
  $b := data→list→is_invalid;
}

