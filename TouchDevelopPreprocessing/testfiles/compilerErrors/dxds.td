meta version "v2.2,nothing";
meta name "Current weather Library";
meta icon "ArrowLarge";
//
meta isLibrary "yes";
meta platform "current";

action main() {
  wall→set_reversed(true);
  // http://w1.weather.gov/xml/current_obs/KDPA.xml
  data→county := "DuPage";
  data→strTitle := "Weather observations for " ∥ data→county;
  // This is the color setup
  data→colorForeground := colors→black;
  data→colorBackground := colors→white;
  // This is the National Oceanic and Atmospheric Administration date.  You can find your local station to get weather from here-> http://w1.weather.gov/xml/current_obs/seek.php?state=il&Find=Find
  data→weatherURL := "http://w1.weather.gov/xml/current_obs/KDPA.xml";
  $s := web→download(data→weatherURL);
  if $s→is_invalid then {
    "Unable to retrieve data via the Internet.  Make sure you are connected to a network." →post_to_wall;
  }
  else {
    code→Parse_all_data($s);
    // Change BoxUI to false to be able to build Wall for Windows Phone
    $BoxUI := false;
    if $BoxUI then {
      code→show_current_weather;
    }
    else {
      code→Build_wall_UI;
    }
  }
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
}

action get_xml_value(inStr: String, tag: String) returns @match: String {
  $@match := "";
  $beginTag := "<" ∥ $tag ∥ ">";
  $endTag := "</" ∥ $tag ∥ ">";
  $@match := code→get_string($inStr, $beginTag, $endTag);
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
}
var StrMap : String_Map {
}

action Build_wall_UI() {
  wall→set_foreground(data→colorForeground);
  wall→set_background(data→colorBackground);
  wall→set_title(data→strTitle);
  wall→set_subtitle(data→StrMap→at("Observation time"));
  foreach e in data→StrMap where true do {
    $s := $e ∥ ": " ∥ data→StrMap→at($e);
    code→Display_textbox($s);
  }
}

action Display_textbox(theText: String) {
  $tb := wall→create_text_box($theText, 19);
  $tb→set_border(colors→foreground);
  $tb→post_to_wall;
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
}
var colorForeground : Color {
}
var colorBackground : Color {
}

action show_current_weather() {
  if box→is_init then {
    wall→set_background(data→colorBackground);
    wall→set_foreground(data→colorForeground);
    wall→set_title(data→strTitle);
    wall→set_subtitle(data→StrMap→at("Observation time"));
  }
  if true then {
    foreach e in data→StrMap where true do {
      code→Display_row($e ∥ ":", data→StrMap→at($e));
    }
  }
}
var strTitle : String {
}
var weatherURL : String {
}
var county : String {
}
table thing {
  type = "Object";
}

action Initialize() {
  wall→set_reversed(true);
  // http://w1.weather.gov/xml/current_obs/KDPA.xml
  data→county := "DuPage";
  data→strTitle := "Weather observations for " ∥ data→county;
  // This is the color setup
  data→colorForeground := colors→black;
  data→colorBackground := colors→white;
  // This is the National Oceanic and Atmospheric Administration date.  You can find your local station to get weather from here-> http://w1.weather.gov/xml/current_obs/seek.php?state=il&Find=Find
  data→weatherURL := "http://w1.weather.gov/xml/current_obs/KDPA.xml";
  $s := web→download(data→weatherURL);
  if $s→is_invalid then {
    "Unable to retrieve data via the Internet.  Make sure you are connected to a network." →post_to_wall;
  }
  else {
    // Change BoxUI to false to be able to build Wall for Windows Phone
  }
}

