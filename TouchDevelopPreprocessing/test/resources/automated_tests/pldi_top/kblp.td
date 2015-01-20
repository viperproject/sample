meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "where am I ??";
meta icon "target";
meta color "#ffff0038";
meta rootId "jJqAJcOyom2LNlZNRZuSTERF";
meta showAd "yes";
meta platform "current";
meta parentIds "";
// This Script reads your current GPS Position from your phone. It then shows you the distinct values on the screen.
//
// These are: Longitude, Latitude, Altitude and Speed.
//
// All Data remains on the phone. Your location will absolutely remain private to you.
#main
action main() {
  code→setup_Wall;
  code→show_privacy_notice;
  code→show_Map(1);
}
#setupWall
action setup_Wall() {
  // Wall einrichten
  wall→set_background(colors→background);
  wall→set_foreground(colors→accent);
  wall→set_title("Where am I ??");
  wall→set_subtitle("According to your GPS you are here: ");
  wall→set_reversed(true);
  "Initializing GPS, reading Data" →post_to_wall;
}
#showMap
action show_Map(zoomin: Number) {
  // Position bestimmen:
  $loc := senses→current_location_accurate;
  // Karte einrichten und anzeigen
  $karte := maps→create_map;
  "Initializing GPS, reading Data" →post_to_wall;
  //:: ExpectedOutput(assert.failed)
  $karte→set_center($loc);
  $karte→set_zoom(18);
  $karte→add_text($loc, locations→describe_location($loc), colors→black, colors→white);
  wall→clear;
  $karte→post_to_wall;
  // Langsam hineinzoomen
  $zoomfaktor := 2;
  $zoomstart := 6;
  if $zoomin = 1 then {
    for 0 ≤ i1 < 7 do {
      $karte→set_zoom($i1 * $zoomfaktor + $zoomstart);
      time→sleep(4);
    }
  }
  else {
    time→sleep(4);
  }
}
#writetoWall
action write_to_Wall() {
  // Position bestimmen:
  $loc := senses→current_location_accurate;
  // Wall beschriften
  wall→push_new_page;
  code→setup_Wall;
  wall→clear;
  $fontsize := 25;
  "Your Position is: " →post_to_wall;
  //:: ExpectedOutput(assert.failed)
  wall→create_text_box(locations→describe_location($loc), $fontsize)→post_to_wall;
  wall→create_text_box("Latitude : " →concat($loc→latitude→to_string), $fontsize)→post_to_wall;
  wall→create_text_box("Longitude : " →concat($loc→longitude→to_string), $fontsize)→post_to_wall;
  wall→create_text_box("Horizontal accuracy: " →concat($loc→hor_accuracy→to_string), $fontsize)→post_to_wall;
  wall→create_text_box("Altitude : " →concat($loc→altitude→to_string), $fontsize)→post_to_wall;
  wall→create_text_box("Vertical accuracy: " →concat($loc→vert_accuracy→to_string), $fontsize)→post_to_wall;
  wall→create_text_box("Speed: " →concat($loc→speed→to_string), $fontsize)→post_to_wall;
  wall→create_text_box("Course : " →concat($loc→course→to_string), $fontsize)→post_to_wall;
  time→sleep(4);
  wall→pop_page;
  code→show_Map(0);
}
#gameloop
event gameloop() {
  code→write_to_Wall;
  meta private;
}
#showprivacynotice
action show_privacy_notice() {
  $@continue := "S";
  wall→set_foreground(colors→foreground);
  wall→set_reversed(false);
  while `not` $@continue→equals("N") `and` `not` $@continue→equals("n") `and` `not` $@continue→equals("Y") `and` `not` $@continue→equals("y") do {
    "Privacy notice:\n\nThis Script reads your current GPS Position from your phone. It then shows you the distinct values on the screen.\n\nThese are: Longitude, Latitude, Altitude and Speed.\n\nAll Data remains on the phone. Your location will absolutely remain private to you." →post_to_wall;
    $@continue := wall→ask_string("continue? (y/n)");
    wall→clear;
  }
  code→setup_Wall;
  if $@continue→equals("Y") `or` $@continue→equals("y") then {
    skip;
  }
  else {
    time→stop;
  }
}