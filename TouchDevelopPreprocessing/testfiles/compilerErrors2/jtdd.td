meta version "v2.2,nothing";
meta name "Illinois Counties Current Weather";
meta icon "BulletList";
meta color "#ffDAA520";
// A little sample showing how to create a list box, using boxes.
meta isLibrary "yes";
meta platform "current";

action main() {
  code→show_county_list;
}

action display_item(thing: item) {
  // Displays an individual item in the list box
  do box {
    // if this is the currently selected item, highlight it
    if not data→selected→is_invalid and $thing→equals(data→selected) then {
      box→set_background(colors→accent);
    }
    // when tapping this item, it becomes selected
    box→on_tapped($handler)
    where handler() {
      data→selected := $thing;
      time→log($thing→Airport→get);
      code→show($thing→Airport→get);
    }
    // we want a bit of margin between items
    box→set_margins(0.3, 0, 0.3, 0);
    // All items have two columns, and are stretched so they are all the same width
    box→use_horizontal_layout;
    box→set_horizontal_alignment(1, 1);
    // first column: the icon
    if false then {
      do box {
        box→set_height(3);
        box→set_width(3);
        box→set_background(colors→orange);
        box→set_font_size(2);
        $thing→Airport→get→post_to_wall;
        box→set_horizontal_alignment(0, 0);
        skip;
      }
    }
    // second column: the description
    do box {
      box→set_margins(0.2, 0.2, 0.2, 0.2);
      ($thing→Airport→get)→post_to_wall;
      // we want this part to stretch so it fills the column
      box→set_horizontal_alignment(1, 1);
    }
  }
  meta private;
}
table item {
  type = "Object";
  fields {
    Airport: String_field
  }
}

action fill_list() {
  // initialize the list with random content
  data→my_list := records→item→create_collection;
  $s := "";
  foreach e in art→pic where true do {
    $s := $s→concat($e);
  }
  time→log($s→split("\n")→count→to_string);
  foreach e2 in $s→split("\n") where true do {
    $item1 := records→item→create;
    $item1→Airport→set($e2);
    data→my_list→add($item1);
  }
  meta private;
}
var my_list : item_Collection {
  // The list of items we are displaying in the list box.
}
var selected : item {
  // The currently selected item.
}

action display_list() {
  // Displays the list box
  do box {
    // Prevent the list from using more than half the page height (it may use less)
    box→set_height_range(0, box→page_height / 2);
    // We want a frame and a vertical scroll bar
    box→set_border(colors→black, 0.1);
    box→set_scrolling(false, true);
    // Now display all the items
    foreach e in data→my_list where true do {
      code→display_item($e);
    }
  }
  meta private;
}

action show_county_list() {
  if box→is_init then {
    code→page_setup;
    code→fill_list;
  }
  if true then {
    box→set_padding(1, 1, 1, 1);
    do box {
      box→use_horizontal_layout;
      "Select a Illinois airport : " →post_to_wall;
      code→display_list;
    }
  }
}
var pic : String {
  is\_resource = true;
  url = "http://www.martinschray.com/IL airports.txt";
}

action Display_row(header: String, content: String) {
  do box {
    box→use_horizontal_layout;
    do box {
      $header→post_to_wall;
      box→set_width(6);
      box→set_font_size(.75);
      box→set_foreground(data→colorForeground→darken(0.5));
      box→set_margins(0.5, 0.5, 0.5, 0.5);
    }
    do box {
      box→set_width(20);
      box→set_font_size(.75);
      box→set_text_wrapping(true, 15);
      box→set_border(colors→black, 0.1);
      box→set_margins(0.5, 0.5, 0.5, 0.5);
      $content→post_to_wall;
    }
  }
  meta private;
}

action get_string(inStr: String, strbegin: String, strEnd: String) returns matchstr: String {
  $matchstr := "";
  $beginLoc := $inStr→index_of($strbegin, 0);
  $endLoc := $inStr→index_of($strEnd, $beginLoc);
  if $beginLoc > - 1 then {
    if $endLoc > - 1 then {
      $matchstr := $inStr→substring($beginLoc + $strbegin→count, $endLoc - $beginLoc - $strEnd→count + 1);
    }
    else {
      $matchstr := "";
    }
  }
  meta private;
}

action get_xml_value(inStr: String, tag: String) returns @match: String {
  $@match := "";
  $beginTag := "<" ∥ $tag ∥ ">";
  $endTag := "</" ∥ $tag ∥ ">";
  $@match := code→get_string($inStr, $beginTag, $endTag);
  meta private;
}

action Parse_all_data(inStr: String) {
  data→StrMap := collections→create_string_map;
  data→StrMap→set_at("Location", code→get_xml_value($inStr, "location"));
  data→StrMap→set_at("Station", code→get_xml_value($inStr, "station_id"));
  data→StrMap→set_at("Latitude", code→get_xml_value($inStr, "latitude"));
  data→StrMap→set_at("Longitude", code→get_xml_value($inStr, "longitude"));
  data→StrMap→set_at("Observation time", code→get_xml_value($inStr, "observation_time"));
  data→StrMap→set_at("Weather", code→get_xml_value($inStr, "weather"));
  data→StrMap→set_at("Temperature", code→get_xml_value($inStr, "temperature_string"));
  data→StrMap→set_at("Wind", code→get_xml_value($inStr, "wind_string"));
  data→StrMap→set_at("Wind direction", code→get_xml_value($inStr, "wind_dir"));
  $@match := code→get_xml_value($inStr, "windchill_string");
  if $@match→is_empty then {
    data→StrMap→set_at("Wind chill", "NA");
  }
  else {
    data→StrMap→set_at("Wind chill", code→get_xml_value($inStr, "windchill_string"));
  }
  data→StrMap→set_at("Visibility (miles)", code→get_xml_value($inStr, "visibility_mi"));
  meta private;
}

action show(airportStr: String) {
  if box→is_init then {
    code→page_setup;
    wall→set_background(data→colorBackground);
    wall→set_foreground(data→colorForeground);
    wall→set_title(data→strTitle);
    $strings := $airportStr→split("(");
    $airportCode := $strings→at(1)→split(")")→at(0);
    time→log($airportCode);
    wall→set_subtitle("Airport call sign: " ∥ $airportCode);
    // This is the National Oceanic and Atmospheric Administration date.  You can find your local station to get weather from here-> http://w1.weather.gov/xml/current_obs/seek.php?state=il&Find=Find
    data→weatherURL := "http://w1.weather.gov/xml/current_obs/" ∥ $airportCode ∥ ".xml";
    $s := web→download(data→weatherURL);
    if $s→is_invalid then {
      "Unable to retrieve data via the Internet.  Make sure you are connected to a network." →post_to_wall;
    }
    else {
      code→Parse_all_data($s);
      // Change BoxUI to false to be able to build Wall for Windows Phone
    }
  }
  if true then {
    foreach e in data→StrMap where true do {
      code→Display_row($e ∥ ":", data→StrMap→at($e));
    }
  }
}
var colorBackground : Color {
}
var colorForeground : Color {
}
var strTitle : String {
}
var county : String {
}
var StrMap : String_Map {
}
var weatherURL : String {
}

action page_setup() {
  data→strTitle := "Weather observations at Illinois Airports";
  $strSubtile := "The weather observations at Illinois Airports";
  wall→set_subtitle($strSubtile);
  wall→set_title(data→strTitle);
  // This is the color setup
  data→colorForeground := colors→black;
  data→colorBackground := colors→white;
  meta private;
}

