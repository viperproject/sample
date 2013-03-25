meta version "v2.2,nothing";
meta name "Todo List";
meta icon "bulletlist";
meta color "#ffff7518";
//
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
  resolve todo_item = ♻ba\u015Fl\u0131\u011Fa_git with {
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection with {
  }
  resolve color_helpers = ♻color_helpers resolve instance = ♻instance resolve definition = ♻definition}
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
meta import ba\u015Fl\u0131\u011Fa_git {
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

action main() {
  code→init;
  code→list;
  ♻ todo_items→delete(0);
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
  data→yeni_buton_koy := wall→create_text_box("Add New Item...", $font_size);
  data→list_header := wall→create_text_box("Select Item:", $font_size);
  data→ba\u015Fl\u0131k_ad\u0131 := wall→create_text_box("", $font_size);
  data→ba\u015Fl\u0131k_a\u00E7\u0131klamas\u0131 := wall→create_text_box("", $font_size);
  data→ba\u015Fl\u0131k_tamam := wall→create_text_box("", $font_size);
  data→item_due_date := wall→create_text_box("", $font_size);
  data→item_due_time := wall→create_text_box("", $font_size);
  data→geri := wall→create_text_box("Back", $font_size);
  meta private;
}

action list() {
  wall→clear;
  data→msgs := ♻ todo_items→to_messages;
  data→yeni_buton_koy→post_to_wall;
  if data→msgs→count = 0 then {
    "Nesne yok" →post_to_wall;
  }
  else {
    data→msgs→post_to_wall;
  }
  data→list_header→post_to_wall;
  meta private;
}
var msgs : Message_Collection {
}
var yeni_buton_koy : TextBox {
}
var list_header : TextBox {
}

event tap_wall_TextBox(item: TextBox) {
  $s := $item→text;
  if $s→equals(data→yeni_buton_koy→text) then {
    $id := ♻ todo_items→add();
    code→details($id);
  }
  else {
    if $s→equals(data→geri→text) then {
      code→list;
    }
    else {
      wall→clear;
      $name := ♻ ba\u015Fl\u0131\u011Fa_git→name(data→ba\u015Fl\u0131k);
      $id1 := ♻ ba\u015Fl\u0131\u011Fa_git→id(data→ba\u015Fl\u0131k);
      if $s→equals(data→ba\u015Fl\u0131k_ad\u0131→text) then {
        $newname := wall→ask_string("Enter new name for " ∥ $name);
        data→ba\u015Fl\u0131k := ♻ ba\u015Fl\u0131\u011Fa_git→set_name(data→ba\u015Fl\u0131k, $newname);
      }
      else {
        if $s→equals(data→ba\u015Fl\u0131k_tamam→text) then {
          data→ba\u015Fl\u0131k := ♻ ba\u015Fl\u0131\u011Fa_git→toggle(data→ba\u015Fl\u0131k);
        }
        else {
          if $s→equals(data→ba\u015Fl\u0131k_a\u00E7\u0131klamas\u0131→text) then {
            $newdesc := wall→ask_string("Enter new description for " ∥ $name);
            data→ba\u015Fl\u0131k := ♻ ba\u015Fl\u0131\u011Fa_git→set_description(data→ba\u015Fl\u0131k, $newdesc);
          }
          else {
            if $s→equals(data→item_due_date→text) then {
              $newdate := wall→pick_date("", "Enter new date for " ∥ $name);
              $dt := ♻ ba\u015Fl\u0131\u011Fa_git→due_date(data→ba\u015Fl\u0131k);
              $h := $dt→hour;
              $m := $dt→minute;
              data→ba\u015Fl\u0131k := ♻ ba\u015Fl\u0131\u011Fa_git→set_due_date(data→ba\u015Fl\u0131k, $newdate→add_hours($h)→add_minutes($m));
            }
            else {
              if $s→equals(data→item_due_time→text) then {
                $newtime := wall→pick_time("", "Enter new time for " ∥ $name);
                $dt1 := ♻ ba\u015Fl\u0131\u011Fa_git→due_date(data→ba\u015Fl\u0131k)→date;
                $h1 := $newtime→hour;
                $m1 := $newtime→minute;
                data→ba\u015Fl\u0131k := ♻ ba\u015Fl\u0131\u011Fa_git→set_due_date(data→ba\u015Fl\u0131k, $dt1→add_hours($h1)→add_minutes($m1));
              }
            }
          }
        }
      }
      ♻ todo_items→edit(data→ba\u015Fl\u0131k);
      code→details($id1);
    }
  }
  meta private;
}
var ba\u015Fl\u0131k_ad\u0131 : TextBox {
}
var ba\u015Fl\u0131k_a\u00E7\u0131klamas\u0131 : TextBox {
}
var item_due_date : TextBox {
}
var ba\u015Fl\u0131k_tamam : TextBox {
}
var item_due_time : TextBox {
}

action details(id: Number) {
  data→ba\u015Fl\u0131k := ♻ todo_items→get_by_id($id);
  data→ba\u015Fl\u0131k_ad\u0131→set_text("Name:  " ∥ ♻ ba\u015Fl\u0131\u011Fa_git→name(data→ba\u015Fl\u0131k));
  data→ba\u015Fl\u0131k_tamam→set_text((♻ ba\u015Fl\u0131\u011Fa_git→complete(data→ba\u015Fl\u0131k)→to_number + 9744)→to_character ∥ "  " ∥ ♻ ba\u015Fl\u0131\u011Fa_git→status(data→ba\u015Fl\u0131k));
  data→ba\u015Fl\u0131k_a\u00E7\u0131klamas\u0131→set_text("Description/Notes:\n" ∥ ♻ ba\u015Fl\u0131\u011Fa_git→description(data→ba\u015Fl\u0131k));
  $dt := ♻ ba\u015Fl\u0131\u011Fa_git→due_date(data→ba\u015Fl\u0131k);
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
  data→geri→post_to_wall;
  data→item_due_time→post_to_wall;
  data→item_due_date→post_to_wall;
  data→ba\u015Fl\u0131k_a\u00E7\u0131klamas\u0131→post_to_wall;
  data→ba\u015Fl\u0131k_tamam→post_to_wall;
  data→ba\u015Fl\u0131k_ad\u0131→post_to_wall;
  meta private;
}
var ba\u015Fl\u0131k : Json_Object {
}
var geri : TextBox {
}

action choose(id: Number) {
  $strings := "Check/Uncheck\nEdit\nDelete\nCancel " →split("\n");
  $s := ♻ ba\u015Fl\u0131\u011Fa_git→name(♻ todo_items→get_by_id($id));
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

action go() {
  skip;
  meta private;
}

