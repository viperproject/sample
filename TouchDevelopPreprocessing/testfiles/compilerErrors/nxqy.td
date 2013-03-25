meta version "v2.2,nothing";
meta name "My Notes";
//
meta platform "current";

action main() {
  wall→set_background(colors→gray);
  wall→set_foreground(colors→green);
  code→buttons("Clear All Data");
  code→buttons("View All Data");
  code→buttons("Add New");
}
table notes {
  type = "Table";
  fields {
    content: String_field
    date: DateTime_field
    loc: Location_field
  }
}

action inputdata(data1: String, timestamp: DateTime) {
  $record := records→notes_table→add_row;
  $record→content→set($data1);
  $record→date→set($timestamp);
  $record→loc→set(senses→current_location_accurate);
  meta private;
}

action outputdata() {
  records→notes_table→post_to_wall;
  meta private;
}

action buttons(name: String) {
  $tb := wall→create_text_box($name, 40);
  $tb→set_background(colors→background→make_transparent(0));
  $tb→set_foreground(colors→green);
  $tb→post_to_wall;
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  if $item→text→equals("Add New") then {
    wall→push_new_page;
    wall→add_button("save", "Save Note");
    data→usrdata := wall→ask_string("Enter Notes:");
  }
  else {
    if $item→text→equals("View All Data") then {
      wall→push_new_page;
      code→outputdata;
      "Record Count::" →concat(records→notes_table→count→to_string)→post_to_wall;
    }
    else {
      if $item→text→equals("Clear All Data") then {
        wall→push_new_page;
        $usresp := wall→ask_boolean("Are you sure you want to clear ALL Data?", "");
        if $usresp then {
          records→notes_table→clear;
          "Record Count::" →concat(records→notes_table→count→to_string)→post_to_wall;
        }
      }
    }
  }
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→icon→equals("save") then {
    code→inputdata(data→usrdata, time→now);
    "Notes saved Successfully" →post_to_wall;
  }
  meta private;
}
var usrdata : String {
}

