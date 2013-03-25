meta version "v2.2,nothing";
meta name "GPS Collections";
//
meta platform "current";

action main() {
  wall→set_reversed(true);
  data→addCurrentTxt := "Add Current Location";
  data→Show_map := "Show Map";
  $page_button := wall→add_button("add", data→addCurrentTxt);
  $tb := wall→create_text_box(data→addCurrentTxt, 19);
  $sm := wall→create_text_box(data→Show_map, 19);
  $tb→post_to_wall;
  $sm→post_to_wall;
  code→ShowSettings;
}
table CollectionPoints {
  // Contains all the collection points
  type = "Table";
  fields {
    place: Place_field
    description: String_field
    picture: Picture_field
    date: DateTime_field
    test: Boolean_field
  }
}
var addCurrentTxt : String {
}

event tap_wall_TextBox(item: TextBox) {
  if $item→text→equals(data→addCurrentTxt) then {
    data→addCurrentTxt→post_to_wall;
    code→AddCurrentLocation;
  }
  else {
    if $item→text→equals(data→Show_map) then {
      code→Display_Map;
    }
    else {
      $item→text→post_to_wall;
      foreach s in records→settings_index where true do {
        if $s→desc→get→equals($item→text) then {
          // Toggle setting
          $b := not $s→value→get;
          $s→value→set($b);
          $s→value→get→post_to_wall;
          if $b then {
            $item→set_foreground(colors→accent);
          }
          else {
            $item→set_foreground(colors→subtle);
          }
        }
      }
    }
  }
  meta private;
}

action AddCurrentLocation() {
  wall→push_new_page;
  wall→set_reversed(true);
  $record := records→CollectionPoints_table→add_row;
  // Saving a description?
  if records→settings_index→at("Description")→value→get then {
    $description := wall→ask_string("What do you call this place?");
    $record→description→set($description);
  }
  // Saving a picture?
  if records→settings_index→at("Picture")→value→get then {
    $pic := media→choose_picture;
    $record→picture→set($pic);
    $pic→resize(100, - 1);
    $pic→post_to_wall;
  }
  // Is this a test result?
  $record→test→set(records→settings_index→at(data→stnTest)→value→get);
  // Store time
  $record→date→set(time→now);
  // Get GPS coordinates
  "Getting GPS data..." →post_to_wall;
  $loc := senses→current_location_accurate;
  $place := social→create_place("", $loc);
  $record→place→set($place);
  wall→pop_page;
  meta private;
}

action Display_Map() {
  wall→push_new_page;
  $map := maps→create_full_map;
  foreach c in records→CollectionPoints_table where true do {
    $date := $c→date→get→to_string→replace_regex("\\d?\\d:\\d*:\\d*\\s*\\wM", "");
    $date := $c→date→get→month→to_string ∥ "/" ∥ $c→date→get→day→to_string ∥ "/";
    $year := $c→date→get→year→to_string;
    if $year→count > 3 then {
      $year := $year→substring(02, $year→count - 2);
    }
    $date := $date ∥ $year;
    $desc := $c→description→get;
    if $desc→is_empty then {
      $desc := $date;
    }
    else {
      $desc := $desc→concat("\n")→concat($date);
    }
    $place := social→create_place($desc, $c→place→get→location);
    $map→add_place($place, colors→background, colors→foreground);
  }
  $map→post_to_wall;
  meta private;
}
var Show_map : String {
}

action ShowSettings() {
  // Initialize settings
  if records→settings_index→count < 3 then {
    $s1 := records→settings_index→at("Description");
    $s1→desc→set("Add Description");
    $s1→value→set(false);
    $s1 := records→settings_index→at("Picture");
    $s1→desc→set("Add Photo");
    $s1→value→set(false);
    data→stnTest := "Test Mode";
    $s1 := records→settings_index→at(data→stnTest);
    $s1→desc→set(data→stnTest);
    $s1→value→set(false);
  }
  // Settings header
  $settingsTxt := wall→create_text_box("\nSettings:", 30);
  $settingsTxt→set_background(colors→background→make_transparent(0));
  $settingsTxt→post_to_wall;
  // Post settings
  foreach s in records→settings_index where true do {
    $setting := wall→create_text_box($s→desc→get, 19);
    $b := $s→value→get;
    if $b then {
      $setting→set_foreground(colors→accent);
    }
    else {
      $setting→set_foreground(colors→subtle);
    }
    $setting→post_to_wall;
  }
  meta private;
}
var stnDesc : String {
}
var stnPic : String {
}
table settings {
  type = "Index";
  keys {
    name: String
  }
  fields {
    desc: String_field
    value: Boolean_field
  }
}
var stnTest : String {
}

