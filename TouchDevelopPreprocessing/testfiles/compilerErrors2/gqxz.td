meta version "v2.2,nothing";
meta name "Weather Alerts for Illinois Counties";
meta icon "BulletList";
meta color "#ffDAA520";
// A little sample showing how to create a list box, using boxes.
meta isLibrary "yes";
meta platform "current";

action main() {
  data→colorForeground := colors→blue;
  data→colorBackground := colors→white;
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
      time→log($thing→CountyID→get);
      code→table_display($thing→CountyID→get, $thing→County→get);
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
        $thing→County→get→post_to_wall;
        box→set_horizontal_alignment(0, 0);
        skip;
      }
    }
    // second column: the description
    do box {
      box→set_margins(0.2, 0.2, 0.2, 0.2);
      ($thing→County→get)→post_to_wall;
      // we want this part to stretch so it fills the column
      box→set_horizontal_alignment(1, 1);
    }
  }
  meta private;
}
table item {
  type = "Object";
  fields {
    County: String_field
    CountyID: String_field
    State: String_field
  }
}

action fill_list_randomly() {
  // initialize the list with random content
  data→my_list := records→item→create_collection;
  $s := "";
  foreach e in art→pic where true do {
    $s := $s→concat($e);
  }
  time→log($s→split("\n")→count→to_string);
  foreach e2 in $s→split("\n") where true do {
    $item1 := records→item→create;
    $strings := $e2→split(";");
    time→log($strings→at(0));
    $item1→CountyID→set($strings→at(0));
    $item1→County→set($strings→at(1));
    $item1→State→set("IL");
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
    wall→set_background(data→colorBackground);
    wall→set_foreground(data→colorForeground);
    wall→set_title("NOAA Weather Alerts of Illinois Counties");
    wall→set_subtitle("Get the latest alerts on severe weather");
    code→fill_list_randomly;
  }
  if true then {
    box→set_padding(1, 1, 1, 1);
    do box {
      box→use_horizontal_layout;
      "Pick a county:   " →post_to_wall;
      code→display_list;
    }
  }
}
var pic : String {
  is\_resource = true;
  url = "http://www.martinschray.com/il%20county%20codes.txt";
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

action Convert_to_validate_date(GMT_Date_Str: String) returns dt2: DateTime {
  // 2013-02-01T01:59:41+00:00
  $StartLoc := $GMT_Date_Str→index_of("<updated>", 0);
  $EndLoc := $GMT_Date_Str→index_of("</updated>", 0);
  $s := $GMT_Date_Str→substring($StartLoc + 9, $EndLoc - $StartLoc - 9);
  $date := $s→index_of("T", 0);
  $x := $s→index_of("+", 0);
  $s2 := $s→substring(0, $date);
  $s3 := $s→substring($date + 1, $s→count - $date - 7);
  $strYear := $s2→substring(0, 4);
  skip;
  $strMonthDay := $s2→substring(5, $s2→count);
  $dt := ($strMonthDay ∥ "-" ∥ $strYear ∥ " " ∥ $s3)→to_datetime;
  if data→is_Debug then {
    time→log("Date=" ∥ $dt→to_string);
  }
  $dt2 := $dt;
  meta private;
}

action Display_row(header3: String, content2: String) {
  do box {
    box→use_horizontal_layout;
    do box {
      $header3→post_to_wall;
      box→set_font_size(.75);
      box→set_width(8);
      box→set_foreground(data→colorForeground);
      box→set_margins(0.5, 0.5, 0.5, 0.5);
    }
    do box {
      box→set_width(20);
      box→set_text_wrapping(true, 15);
      box→set_font_size(.75);
      box→set_border(data→colorForeground, .1);
      box→set_margins(0.5, 0.5, 0.5, 0.5);
      $content2→post_to_wall;
    }
  }
  meta private;
}

action Parse_Alert_Text(s: String) returns s2: String {
  $endtitle := $s→last_index_of("title", $s→count);
  $starttitle := $s→last_index_of("title", $endtitle - 1);
  $AlertStr := $s→substring($starttitle + 6, $endtitle - $starttitle - 8);
  if data→is_Debug then {
    time→log("AlertStr=" ∥ $AlertStr);
  }
  $s2 := $AlertStr;
  meta private;
}

action Parse_all_data(NOAAURL: String) {
  $s3 := code→Read_Weather_Alert($NOAAURL);
  data→alertStr := code→Parse_Alert_Text($s3);
  time→log($s3);
  data→summary := code→Parse_Summary($s3);
  data→location := code→Parse_Location($s3);
  data→AreaAffected := code→Parse_Effected_Area($s3);
  data→dt2 := code→Convert_to_validate_date($s3);
  if data→summary→equals("") then {
    data→summary := "No alerts";
  }
  skip;
  if data→AreaAffected→equals("") then {
    data→AreaAffected := "None";
  }
  meta private;
}

action Parse_Effected_Area(s: String) returns s3: String {
  // <cap:areaDesc>Cook; DuPage; Kane; Kendall; Will</cap:areaDesc>
  $endtitle := $s→index_of("</cap:areaDesc>", 0);
  $starttitle := $s→index_of("<cap:areaDesc>", 0);
  $AlertStr := $s→substring($starttitle + 14, $endtitle - $starttitle - 14);
  if data→is_Debug then {
    time→log($starttitle→to_string);
    time→log($endtitle→to_string);
    time→log("Effected area=" ∥ $AlertStr);
  }
  $s3 := $AlertStr;
  meta private;
}

action Parse_Location(s: String) returns s2: String {
  $endtitle := $s→index_of("</title>", 0);
  $starttitle := $s→last_index_of("<title>", $endtitle - 1);
  $AlertStr := $s→substring($starttitle + 7, $endtitle - $starttitle - 7);
  if data→is_Debug then {
    time→log("Location=" ∥ $AlertStr);
  }
  time→log($AlertStr);
  $s2 := $AlertStr;
  meta private;
}

action Parse_Summary(s: String) returns s2: String {
  $endtitle := $s→index_of("</summary>", 0);
  $starttitle := $s→index_of("<summary>", 0);
  $AlertStr := $s→substring($starttitle + 9, $endtitle - $starttitle - 9);
  if data→is_Debug then {
    time→log("Summary=" ∥ $AlertStr);
  }
  $s2 := $AlertStr;
  meta private;
}

action Read_Weather_Alert(URLToRead: String) returns s2: String {
  $s := web→download($URLToRead);
  $s2 := $s;
  meta private;
}
var is_Debug : Boolean {
}
var alertStr : String {
}
var summary : String {
}
var location : String {
}
var AreaAffected : String {
}
var dt2 : DateTime {
}

action table_display(countyCode: String, County: String) {
  if box→is_init then {
    data→is_Debug := true;
    $s := "<?xml version = \'1.0\' encoding = \'UTF-8\' standalone = \'yes\'?>\n<!-- This atom/xml feed is an index to active advisories, watches and warnings issued by the National Weather Service. This index file is not the complete Common Alerting Protocol (CAP) alert message. To obtain the complete CAP alert, please follow the links for each entry in this index. Also note the CAP message uses a style sheet to convey the information in a human readable format. Please view the source of the CAP message to see the complete data set. Not all information in the CAP message is contained in this index of active alerts. -->\n<feed xmlns = \'http://www.w3.org/2005/Atom\' xmlns:cap = \'urn:oasis:names:tc:emergency:cap:1.1\' xmlns:ha = \'http://www.alerting.net/namespace/index_1.0\' >\n  <!-- TZN = <CST> -->\n  <!-- TZO = <-6> -->\n  <!-- http-date = Thu, 31 Jan 2013 11:26:00 GMT -->\n  <id>http://alerts.weather.gov/cap/wwaatmget.php?x=ILZ013&amp;y=0</id>\n  <generator>NWS CAP Server</generator>\n  <updated>2013-01-31T17:26:00-06:00</updated>\n  <author>\n    <name>w-nws.webmaster@noaa.gov</name>\n  </author>\n  <title>Current Watches, Warnings and Advisories for DuPage (ILZ013) Illinois Issued by the National Weather Service</title>\n  <link href=\'http://alerts.weather.gov/cap/wwaatmget.php?x=ILZ013&amp;y=0\'/>\n  <entry>\n    <id>http://alerts.weather.gov/cap/wwacapget.php?x=IL124EE9524758.SpecialWeatherStatement.124EED7B052CIL.LOTSPSLOT.6170b62ec908c19bf59c59f58f6675d9</id>\n    <updated>2013-01-31T17:26:00-06:00</updated>\n    <published>2013-01-31T17:26:00-06:00</published>\n    <author>\n      <name>w-nws.webmaster@noaa.gov</name>\n    </author>\n    <title>Special Weather Statement issued January 31 at 5:26PM CST by NWS</title>\n    <link href=\"http://alerts.weather.gov/cap/wwacapget.php?x=IL124EE9524758.SpecialWeatherStatement.124EED7B052CIL.LOTSPSLOT.6170b62ec908c19bf59c59f58f6675d9\"/>\n    <summary>...LIGHT TO MODERATE SNOW EXPECTED THIS EVENING... LATEST RADAR IMAGERY DEPICTING FURTHER DEVELOPMENT OF A COUPLE NARROW BANDS OF MORE MODERATE SNOW EMBEDDED WITHIN THE PERSISTENT LIGHT SNOW LATE THIS AFTERNOON. OF PARTICULAR INTEREST IS A NARROW BAND OF SNOW EXTENDING FROM SOUTH ELGIN SOUTHEAST THROUGH WHEATON...AND TO THE MIDWAY AREA WHERE MODERATE SNOW IS PRODUCING</summary>\n    <cap:event>Special Weather Statement</cap:event>\n    <cap:effective>2013-01-31T17:26:00-06:00</cap:effective>\n    <cap:expires>2013-01-31T19:15:00-06:00</cap:expires>\n    <cap:status>Actual</cap:status>\n    <cap:msgType>Alert</cap:msgType>\n    <cap:category>Met</cap:category>\n    <cap:urgency>Expected</cap:urgency>\n    <cap:severity>Minor</cap:severity>\n    <cap:certainty>Observed</cap:certainty>\n    <cap:areaDesc>Cook; DuPage; Kane; Kendall; Will</cap:areaDesc>\n    <cap:polygon></cap:polygon>\n    <cap:geocode>\n      <valueName>FIPS6</valueName>\n      <value>017031 017043 017089 017093 017197</value>\n      <valueName>UGC</valueName>\n      <value>ILZ012 ILZ013 ILZ014 ILZ020 ILZ022</value>\n    </cap:geocode>\n    <cap:parameter>\n      <valueName>VTEC</valueName>\n      <value></value>\n    </cap:parameter>\n  </entry>\n</feed>";
    // This is where you can setup your customizations
    data→strTitle := "NOAA Weather Alerts for " ∥ $County ∥ " County Illinois";
    wall→set_reversed(true);
    // This the url location of the weather alerts for a particular county.  Go to http://alerts.weather.gov/ to find you county.
    $NOAAURL := "http://alerts.weather.gov/cap/wwaatmget.php?x=" ∥ $countyCode ∥ "&y=0";
    code→Parse_all_data($NOAAURL);
    // Select the type of display below.  If you want to build this app for Windows Phone set BoxUI to false
    wall→set_title(data→strTitle);
    wall→set_subtitle("Last updated: " ∥ data→dt2→to_string);
    wall→set_background(data→colorBackground);
    wall→set_foreground(data→colorForeground);
  }
  if true then {
    code→Display_row("Alert Status:", data→alertStr);
    code→Display_row("Summary:", data→summary);
    code→Display_row("Location:", data→location);
    code→Display_row("Area of Alert:", data→AreaAffected);
    code→Display_row("Last update:", data→dt2→to_string);
  }
}

