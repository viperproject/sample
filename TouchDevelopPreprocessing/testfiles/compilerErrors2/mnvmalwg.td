meta version "v2.2,nothing";
meta name "iZone";
meta icon "location";
meta color "#ff007fff";
// Find Wikipedia articles on places nearby.
meta platform "current,location";
meta import button_library {
  pub "wool"
  usage {
    action create_button(board1: Board, parameters: String_Map) returns button: Sprite
  }
}
meta import check_box_library {
  pub "qghj"
  usage {
    action create_check_box(label_text: String) returns check_box: TextBox
    action toggle_check_box(tb1: TextBox) returns b1: Boolean
  }
}
meta import transitions {
  pub "ezht"
  usage {
    action slide_right_off()
    action slide_left_on()
    action slide_right_on()
    action slide_left_off()
  }
}

action main() {
  code→initialize;
  wall→display_search(true);
  $loc := code→get_location;
  if $loc→distance(data→recent_location) < 1 then {
    data→recently_found_places→post_to_wall;
  }
  else {
    data→recent_location := $loc;
    $s2 := code→get_dbpedia_pages($loc);
    $msgs := code→create_collection_from_xml($s2, $loc);
    data→recently_found_places := $msgs;
    data→recently_found_places→@reverse;
  }
  code→draw_list_page_menu;
}
var buttons : Sprite_Set {
}

event tap_wall_Link(item: Link) {
  $item→name→post_to_wall;
  $item→address→post_to_wall;
  meta private;
}

event gameloop() {
  if math→mod(time→now→second, 2) = 0 then {
    $b := code→task_add_photo_link();
  }
  meta private;
}

event tap_wall_Message(item: Message) {
  data→currently_viewed_msg := $item;
  ♻ transitions→slide_left_off;
  wall→clear;
  code→draw_item_page_menu;
  $item→message→post_to_wall;
  $tb := wall→create_text_box($item→title, 40);
  $tb→set_background(colors→background);
  $tb→post_to_wall;
  if not $item→picture_link→is_invalid then {
    if not $item→picture_link→is_empty then {
      web→download_picture($item→picture_link)→post_to_wall;
    }
  }
  ♻ transitions→slide_left_on;
  meta private;
}

action get_location() returns loc: Location {
  $loc := senses→current_location_accurate;
  $count := 0;
  while $loc→hor_accuracy > 1100 and $count < 6 do {
    time→sleep(2);
    $loc := senses→current_location_accurate;
    $count := $count + 1;
  }
  meta private;
}

action get_dbpedia_pages(loc: Location) returns s2: String {
  $prefix_string := "PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>\nSELECT ?subject ?label ?lat ?long (bif:st_distance(?geo, bif:st_point({0},{1}))) ?abstract\nWHERE {\n?subject geo:geometry ?geo.\n?subject geo:lat ?lat.\n?subject geo:long ?long.\n?subject rdfs:label ?label.\n?subject rdfs:comment ?abstract.";
  $filter_string := "FILTER (bif:st_intersects (?geo, bif:st_point({0}, {1}),{2}) && lang(?label) = \"en\" && lang(?abstract) = \"en\")\n}\nORDER BY ASC 5 LIMIT 15\n";
  $base_URL := "http://dbpedia.org/sparql?query=";
  $prefix_string := $prefix_string→replace("{0}", math→round_with_precision($loc→longitude, 6)→to_string);
  $prefix_string := $prefix_string→replace("{1}", math→round_with_precision($loc→latitude, 6)→to_string);
  $filter_string := $filter_string→replace("{0}", math→round_with_precision($loc→longitude, 6)→to_string);
  $filter_string := $filter_string→replace("{1}", math→round_with_precision($loc→latitude, 6)→to_string);
  $filter_string := $filter_string→replace("{2}", math→round(data→radius)→to_string);
  $request := web→create_request($base_URL ∥ web→url_encode($prefix_string ∥ $filter_string));
  $response := $request→send;
  if $response→status_code ≠ 200 then {
    $response→status_code→post_to_wall;
  }
  ... ;
  $s2 := $response→content;
  $s2 := "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" ∥ $s2;
  meta private;
}
var recently_found_places : Message_Collection {
}

action create_collection_from_xml(s2: String, loc: Location) returns msgs: Message_Collection {
  $xml := web→xml($s2);
  $places := collections→create_place_collection;
  foreach xml1 in $xml where $xml1→name→ends_with("results") do {
    foreach xml2 in $xml1 where $xml2→name→contains("result") do {
      $place := social→create_place("", $loc);
      $temp_lat := 0;
      foreach xml4 in $xml2 where $xml4→attr("name")→equals("label") do {
        $place→set_name($xml4→value);
        ... ;
        $s5 := $xml4→value;
        ... ;
      }
      foreach xml5 in $xml2 where $xml5→attr("name")→equals("lat") do {
        $temp_lat := $xml5→value→to_number;
      }
      foreach xml6 in $xml2 where $xml6→attr("name")→equals("long") do {
        if $temp_lat ≠ 0 then {
          $loc1 := locations→create_location($temp_lat, $xml6→value→to_number);
          $place→set_location($loc1);
        }
      }
      foreach xml3 in $xml2 where $xml3→attr("name")→equals("abstract") do {
        $place→set_category($xml3→value);
      }
      foreach xml7 in $xml2 where $xml7→attr("name")→equals("subject") do {
        $place→set_link($xml7→value→replace("dbpedia.org/resource", "en.wikipedia.org/wiki"));
      }
      ... ;
      $places→add($place);
    }
  }
  $msgs := collections→create_message_collection;
  $places→count→post_to_wall;
  $places→sort_by_distance($loc);
  $places→@reverse;
  foreach place1 in $places where true do {
    $msg := social→create_message($place1→category);
    $msg→set_link($place1→link);
    $msg→set_title($place1→name);
    $msg→set_time(time→now);
    $msg→set_from($place1→name);
    $msg→set_location($place1→location);
    $msg→set_source("Wikipedia");
    $msg→set_to("To");
    $msgs→add($msg);
    $msg→post_to_wall;
    $msg;
  }
  meta private;
}

action arrow() returns pic1: Picture {
  $pic1 := media→create_picture(96, 96);
  $s := 8601→to_character;
  $pic1→draw_text(0, 0, $s, 96, 0, colors→red);
  $pic1→update_on_wall;
  meta private;
}

action map_collection(msgs: Message_Collection) {
  $map := maps→create_full_map;
  $map→set_zoom(18);
  foreach msg in $msgs where true do {
    $map→add_message($msg, colors→background, colors→foreground);
    ... ;
  }
  $map→set_center($msgs→at(0)→location);
  $map→post_to_wall;
  meta private;
}
var saved_places : Message_Collection {
}

action task_add_photo_link() returns task_completed: Boolean {
  $task_completed := false;
  $count := 0;
  while $task_completed→equals(false) and $count < data→recently_found_places→count do {
    if data→recently_found_places→at($count)→picture_link→is_invalid then {
      $s := data→recently_found_places→at($count)→title;
      $loc := data→recently_found_places→at($count)→location;
      $links := web→search_images_nearby($s, $loc, 1000);
      data→recently_found_places→at($count)→set_picture_link($links→at(0)→address);
      $task_completed := true;
    }
    $count := $count + 1;
  }
  meta private;
}
var directions_button : Sprite {
}
var search_button : Sprite {
}

event tap_sprite_in_buttons(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $sprite→equals(data→directions_button) then {
    maps→open_directions("", senses→current_location, "", data→currently_viewed_msg→location);
  }
  if $sprite→equals(data→search_button) then {
    $links := web→search(data→currently_viewed_msg→title);
    wall→clear;
    $links→post_to_wall;
  }
  if $sprite→equals(data→share_button) then {
    data→currently_viewed_msg→share("email");
  }
  if $sprite→equals(data→exit_button) then {
    ♻ transitions→slide_right_off;
    wall→clear;
    code→post_recently_found_places_to_wall;
    ♻ transitions→slide_right_on;
  }
  if $sprite→equals(data→map_button) then {
    code→map_collection(data→recently_found_places);
  }
  if $sprite→equals(data→refresh_button) then {
    ♻ transitions→slide_right_off;
    wall→clear;
    data→recent_location := locations→create_location(90, 0);
    code→main;
    ♻ transitions→slide_right_on;
  }
  if $sprite→equals(data→expand_button) then {
    code→change_search_radius;
    data→recent_location := locations→create_location(90, 0);
    code→main;
  }
  meta private;
}
var currently_viewed_msg : Message {
}
var map_button : Sprite {
}
var expand_button : Sprite {
}

action map_messages(msgs1: Message_Collection) {
  $map := maps→create_full_map;
  foreach msg in $msgs1 where true do {
    $map→add_message($msg, colors→background, colors→foreground);
  }
  $map→post_to_wall;
  $map→view_pushpins;
  meta private;
}
var share_button : Sprite {
}

action draw_list_page_menu() {
  $board := media→create_board(100);
  data→buttons := $board→create_sprite_set;
  $options := collections→create_string_map;
  $options→set_at("icon", "expand");
  $options→set_at("label", "radius");
  data→expand_button := ♻ button_library→create_button($board, $options);
  data→expand_button→set_pos(48, 48);
  data→expand_button→show;
  data→buttons→add(data→expand_button);
  $options→set_at("icon", "map");
  $options→set_at("label", "map");
  data→map_button := ♻ button_library→create_button($board, $options);
  data→map_button→set_pos(150, 48);
  data→map_button→show;
  data→buttons→add(data→map_button);
  $options→set_at("icon", "cycle");
  $options→set_at("label", "refresh");
  data→refresh_button := ♻ button_library→create_button($board, $options);
  data→refresh_button→set_pos(252, 48);
  data→refresh_button→show;
  data→buttons→add(data→refresh_button);
  $board→post_to_wall;
  $board→update_on_wall;
  meta private;
}

action draw_item_page_menu() {
  $board := media→create_board(100);
  $options := collections→create_string_map;
  $options→set_at("icon", "directions");
  $options→set_at("label", "Directions");
  $options→set_at("y", "48");
  $options→set_at("x", "48");
  data→directions_button := ♻ button_library→create_button($board, $options);
  data→buttons := $board→create_sprite_set;
  $b := data→buttons→add(data→directions_button);
  $options→set_at("icon", "search");
  $options→set_at("label", "search");
  $options→set_at("x", "150");
  data→search_button := ♻ button_library→create_button($board, $options);
  $b := data→buttons→add(data→search_button);
  $options→set_at("icon", "share");
  $options→set_at("label", "share");
  $options→set_at("x", "252");
  data→share_button := ♻ button_library→create_button($board, $options);
  $b := data→buttons→add(data→share_button);
  ... ;
  $options→set_at("icon", "exit");
  $options→set_at("label", "back");
  $options→set_at("x", "354");
  data→exit_button := ♻ button_library→create_button($board, $options);
  $b := data→buttons→add(data→exit_button);
  data→search_button→show;
  data→directions_button→show;
  data→share_button→show;
  data→exit_button→show;
  $board→update_on_wall;
  $board→post_to_wall;
  meta private;
}
var exit_button : Sprite {
}

action post_recently_found_places_to_wall() {
  data→recently_found_places→post_to_wall;
  code→draw_list_page_menu;
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  $b := ♻ check_box_library→toggle_check_box($item);
  meta private;
}

action select_places() {
  data→selected_places := collections→create_message_collection;
  foreach msg in data→recently_found_places where true do {
    $tb := ♻ check_box_library→create_check_box($msg→title);
    $tb→post_to_wall;
  }
  meta private;
}
var selected_places : Message_Collection {
}
var refresh_button : Sprite {
}

action initialize() {
  if data→radius→is_invalid then {
    data→radius := 1;
  }
  if data→radius = 0 then {
    data→radius := 1;
  }
  if data→recent_location→is_invalid then {
    data→recent_location := locations→create_location(90, 0);
  }
  meta private;
}
var radius : Number {
}
var recent_location : Location {
}

action change_search_radius() {
  $s := "Current search radius is ";
  $s := $s ∥ data→radius ∥ " km. ";
  $s→post_to_wall;
  $x := wall→ask_number("Enter new search radius in km:");
  if $x > 0 then {
    data→radius := $x;
  }
  else {
    data→radius := 1;
  }
}

