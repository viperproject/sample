meta version "v2.2,nothing";
meta name "To Do List";
meta icon "bulletlist";
meta color "#ffff7518";
// List of todo items. Although web capability is required, data is NEVER used. You can use this script with your data connection off.
meta platform "current";
meta import todo_items {
  pub "vjuv"
  usage {
    action new()
    action add() returns id: Number
    action get_by_id(id: Number) returns item: Json_Object
    action to_messages() returns msgs: Message_Collection
    action toggle(id: Number)
    action delete(id: Number)
    action edit(item: Json_Object)
    action is_invalid() returns b: Boolean
  }
  resolve todo_item = ♻todo_item with {
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection with {
  }
  resolve instance = ♻instance resolve color_helpers = ♻color_helpers resolve definition = ♻definition}
meta import todo_item {
  pub "cpfp"
  usage {
    action name(item: Json_Object) returns s: String
    action id(item: Json_Object) returns n: Number
    action description(item: Json_Object) returns s: String
    action status(item: Json_Object) returns s: String
    action complete(item: Json_Object) returns b: Boolean
    action due_date(item: Json_Object) returns dt: DateTime
    action set_description(item: Json_Object, s: String) returns updated: Json_Object
    action set_due_date(item: Json_Object, dt: DateTime) returns updated: Json_Object
    action set_name(item: Json_Object, s: String) returns updated: Json_Object
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
  }
  resolve core = ♻core with {
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
meta import color_helpers {
  pub "wszv"
  usage {
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

action main() {
  wall→set_title("To Do List");
  code→init;
  code→list;
  wall→add_button("add", "Add Task");
}

event tap_wall_Message(item: Message) {
  wall→clear;
  code→choose($item→to→to_number);
  meta private;
}

action init() {
  if♻ todo_items→is_invalid then {
    ♻ todo_items→new;
  }
  $font_size := 24;
  data→list_header := wall→create_text_box("Tasks:", 32);
  data→item_name := wall→create_text_box("", $font_size);
  data→item_description := wall→create_text_box("", $font_size);
  data→item_complete := wall→create_text_box("", $font_size);
  data→item_due_date := wall→create_text_box("", $font_size);
  data→item_due_time := wall→create_text_box("", $font_size);
  data→back := wall→create_text_box("Back", $font_size);
  meta private;
}

action list() {
  wall→clear;
  data→msgs := ♻ todo_items→to_messages;
  if data→msgs→count = 0 then {
    "Nothing here to do!" →post_to_wall;
  }
  else {
    data→msgs→post_to_wall;
  }
  data→list_header→post_to_wall;
  meta private;
}
var msgs : Message_Collection {
}
var add_new_btn : TextBox {
}
var list_header : TextBox {
}

event tap_wall_TextBox(item: TextBox) {
  $s := $item→text;
  if $s→equals(data→add_new_btn→text) then {
    $id := ♻ todo_items→add();
    code→details($id);
  }
  else {
    if $s→equals(data→back→text) then {
      code→list;
    }
    else {
      wall→clear;
      $name := ♻ todo_item→name(data→item);
      $id1 := ♻ todo_item→id(data→item);
      if $s→equals(data→item_name→text) then {
        $newname := wall→ask_string("Enter new name for " ∥ $name);
        data→item := ♻ todo_item→set_name(data→item, $newname);
      }
      else {
        if $s→equals(data→item_complete→text) then {
          data→item := ♻ todo_item→toggle(data→item);
        }
        else {
          if $s→equals(data→item_description→text) then {
            $newdesc := wall→ask_string("Enter new description for " ∥ $name);
            data→item := ♻ todo_item→set_description(data→item, $newdesc);
          }
          else {
            if $s→equals(data→item_due_date→text) then {
              $newdate := wall→pick_date("", "Enter new date for " ∥ $name);
              $dt := ♻ todo_item→due_date(data→item);
              $h := $dt→hour;
              $m := $dt→minute;
              data→item := ♻ todo_item→set_due_date(data→item, $newdate→add_hours($h)→add_minutes($m));
            }
            else {
              if $s→equals(data→item_due_time→text) then {
                $newtime := wall→pick_time("", "Enter new time for " ∥ $name);
                $dt1 := ♻ todo_item→due_date(data→item)→date;
                $h1 := $newtime→hour;
                $m1 := $newtime→minute;
                data→item := ♻ todo_item→set_due_date(data→item, $dt1→add_hours($h1)→add_minutes($m1));
              }
            }
          }
        }
      }
      ♻ todo_items→edit(data→item);
      code→details($id1);
    }
  }
  meta private;
}
var item_name : TextBox {
}
var item_description : TextBox {
}
var item_due_date : TextBox {
}
var item_complete : TextBox {
}
var item_due_time : TextBox {
}

action details(id: Number) {
  data→item := ♻ todo_items→get_by_id($id);
  data→item_name→set_text("Name:  " ∥ ♻ todo_item→name(data→item));
  data→item_complete→set_text((♻ todo_item→complete(data→item)→to_number + 9744)→to_character ∥ "  " ∥ ♻ todo_item→status(data→item));
  data→item_description→set_text("Description/Notes:\n" ∥ ♻ todo_item→description(data→item));
  $dt := ♻ todo_item→due_date(data→item);
  $d := $dt→date→to_string;
  data→item_due_date→set_text("Date:  " ∥ $d→substring(0, $d→index_of(" ", 0)));
  $s := $dt→minute→to_string;
  while $s→count < 2 do {
    $s := "0" ∥ $s;
  }
  $h := math→mod($dt→hour, 12);
  $hr := $h→to_string;
  if $h = 0 then {
    $hr := 12→to_string;
  }
  $s := $hr ∥ ":" ∥ $s ∥ " ";
  data→item_due_time→set_text("Time:  " ∥ $s ∥ "AP" →at(($dt→hour - $h) / 12) ∥ "M");
  wall→clear;
  data→back→post_to_wall;
  data→item_due_time→post_to_wall;
  data→item_due_date→post_to_wall;
  data→item_description→post_to_wall;
  data→item_complete→post_to_wall;
  data→item_name→post_to_wall;
  meta private;
}
var item : Json_Object {
}
var back : TextBox {
}

action choose(id: Number) {
  $strings := "Check/Uncheck\nEdit\nDelete\nCancel " →split("\n");
  $s := ♻ todo_item→name(♻ todo_items→get_by_id($id));
  wall→clear;
  $x := wall→pick_string("", $s, $strings);
  if $x = 0 then {
    ♻ todo_items→toggle($id);
    code→list;
  }
  if $x = 1 then {
    code→details($id);
  }
  else {
    if $x = 2 then {
      wall→clear;
      if wall→ask_boolean("Delete " ∥ $s, "Are you sure?") then {
        ♻ todo_items→delete($id);
      }
      code→list;
    }
    else {
      code→list;
    }
  }
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  $id := ♻ todo_items→add();
  if $item→text→equals("Add Task") then {
    code→details($id);
  }
  else {
    if $item→text→equals("Delete") then {
      if data→checked→equals(true) then {
        ♻ todo_items→delete($id);
      }
    }
  }
  meta private;
}
var checked : Boolean {
}

