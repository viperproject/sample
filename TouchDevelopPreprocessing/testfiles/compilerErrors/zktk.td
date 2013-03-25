meta version "v2.2,nothing";
meta name "My Pic Notes";
//
meta platform "current";

action main() {
  wall→create_text_box("Clear All", 40)→post_to_wall;
  wall→create_text_box("View All", 40)→post_to_wall;
  wall→create_text_box("Add New", 40)→post_to_wall;
}
table picalbum {
  type = "Table";
  fields {
    pic: Picture_field
    time: DateTime_field
    loc: Location_field
  }
}

action adddata(@table: picalbum) {
  $camera := senses→take_camera_picture;
  $@table→pic→set($camera);
  $@table→time→set(time→now);
  $@table→loc→set(senses→current_location_accurate);
  meta private;
}

action dispdata() {
  foreach p in records→picalbum_table where true do {
    $pic := $p→pic→get;
    $pic→post_to_wall;
    "Pic Taken At:" →concat(locations→describe_location($p→loc→get))→post_to_wall;
    "Pic taken on: " →concat($p→time→get→to_string)→post_to_wall;
  }
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  if $item→text→equals("Add New") then {
    wall→push_new_page;
    $@table := records→picalbum_table→add_row;
    code→adddata($@table);
    "Data Saved Successfully..." →post_to_wall;
  }
  else {
    if $item→text→equals("View All") then {
      wall→push_new_page;
      code→dispdata;
      "Total Record Count:" →concat(records→picalbum_table→count→to_string)→post_to_wall;
    }
    else {
      if $item→text→equals("Clear All") then {
        $conf := wall→ask_boolean("Are You Sure You want to delete ALL data??", "");
        if $conf then {
          records→picalbum_table→clear;
          "All Records Cleared. Record Count: " →concat(records→picalbum_table→count→to_string)→post_to_wall;
        }
      }
    }
  }
  meta private;
}

