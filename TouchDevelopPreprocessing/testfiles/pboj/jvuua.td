meta version "v2.2,nothing";
meta name "my bird log";
meta icon "binoculars";
meta color "#ffa52a2a";
// Browse bird species and track your sightings.
meta platform "current";
table sighting {
  type = "Table";
  fields {
    species_id: Number_field
    date: DateTime_field
    location: Location_field
    color: Color_field
  }
}
table species {
  type = "Index";
  keys {
    id: Number
  }
  fields {
    latin_name: String_field
    english_name: String_field
    sightings_count: Number_field
    short_name: String_field
  }
}

action show_all_species() {
  code→main;
  meta private;
}

action new_sighting(id: Number) {
  $sightings1 := records→sighting_table→add_row;
  $sightings1→species_id→set($id);
  $sightings1→date→set(time→now);
  $sightings1→location→set(senses→current_location);
  $sightings1→color→set(colors→random);
  $s1 := records→species_index→at($id);
  $s1→sightings_count→add(1);
  meta private;
}

action show_sightings_on_map(id: Number) {
  $map := maps→create_map;
  foreach s2 in records→sighting_table where $s2→species_id→get = $id do {
    $place := social→create_place($s2→date→get→to_string, $s2→location→get);
    $map→add_place($place, $s2→color→get, code→contrast($s2→color→get));
  }
  $map→post_to_wall;
  meta private;
}

action download_species_index() {
  // From www.birdlife.com (BirdLife International)
  wall→push_new_page;
  wall→set_reversed(true);
  wall→set_title("Download");
  $request := web→create_request("Http://www.birdlife.org/datazone/speciessearchresults.php?reg=0&cty=222&dmp=SpeciesCSV");
  "Downloading from BirdLife International...\n(www.birdlife.org)" →post_to_wall;
  $response := $request→send;
  $strings := $response→content→split("\n");
  $species_count := $strings→count;
  ("Downloaded " ∥ $species_count→to_string ∥ " species.")→post_to_wall;
  "Building species index..." →post_to_wall;
  for 0 ≤ i < $species_count - 2 do {
    $fields := $strings→at($i + 1)→split(9→to_character);
    $s1 := records→species_index→at($fields→at(0)→to_number);
    $s1→latin_name→set($fields→at(1));
    $s1→english_name→set($fields→at(2));
    $strings1 := $s1→english_name→get→replace("-", " ")→split(" ");
    $s2 := $strings1→at($strings1→count - 1);
    $s1→short_name→set($s2);
  }
  "Building short name index..." →post_to_wall;
  code→make_short_name_index;
  wall→pop_page;
  wall→clear;
  code→main;
  meta private;
}

action show_sightings_list(id: Number) {
  foreach s in records→sighting_table where $s→species_id→get = $id do {
    $tb := wall→create_text_box("", 4);
    $tb→set_background($s→color→get);
    $tb→post_to_wall;
    $tb := wall→create_text_box("Time: " ∥ $s→date→get→to_string, 22);
    $tb := wall→create_text_box("Time: " ∥ $s→date→get→to_string ∥ "\nLocation:" ∥ $s→location→get→to_string, 22);
    $tb→post_to_wall;
  }
  meta private;
}

action show_master1_page() {
  data→level1 := wall→push_new_page;
  wall→set_reversed(true);
  for 0 ≤ i < 26 do {
    $s := ($i + 65)→to_character;
    code→make_letter_box($s)→post_to_wall;
  }
  meta private;
}
var level1 : Page {
  // The page showing the first level (letters)
}

event tap_wall_TextBox(item: TextBox) {
  $page := wall→current_page;
  if $page→equals(data→start_page) then {
    $b := $item→text;
    if $b→starts_with("browse") then {
      code→show_master1_page;
    }
    else {
      code→show_about_page;
    }
  }
  else {
    if $page→equals(data→level1) then {
      $letter := $item→text;
      code→show_master2_page($letter);
    }
    else {
      if $page→equals(data→level2) then {
        $short_name1 := $item→text;
        code→show_master3_page($short_name1);
      }
      else {
        if $page→equals(data→level3) then {
          $t := records→TextBox_decorator→at($item)→species_id→get;
          if not ($t = 0) then {
            code→show_detail_page($t);
          }
        }
        else {
          if $page→equals(data→detail) then {
            if $item→text→contains("map") then {
              code→show_sightings_on_map(data→id);
            }
            else {
              if $item→text→contains("pictures") then {
                code→show_pics_page;
              }
            }
          }
        }
      }
    }
  }
  meta private;
}

action show_master2_page(letter: String) {
  data→level2 := wall→push_new_page;
  wall→set_reversed(true);
  wall→set_title($letter);
  foreach s in records→short_name_index where $s→short_name→starts_with($letter) do {
    $s→_box→get→post_to_wall;
  }
  meta private;
}
var level2 : Page {
}

action make_letter_box(s: String) returns tb1: TextBox {
  $tb1 := wall→create_text_box($s, 45);
  $tb1→set_background(colors→cyan);
  $tb1→set_foreground(colors→black);
  $tb1→set_text($s);
  meta private;
}

action make_short_name_box(s: String) returns tb1: TextBox {
  $tb1 := wall→create_text_box($s, 35);
  $tb1→set_background(colors→magenta);
  $tb1→set_foreground(colors→black);
  meta private;
}
table short_name {
  type = "Index";
  keys {
    short_name: String
  }
  fields {
    _box: TextBox_field
  }
}

action make_short_name_index() {
  foreach s in records→species_index where true do {
    $s1 := records→short_name_index→at($s→short_name→get);
    if $s1→_box→get→is_invalid then {
      $s1→_box→set(code→make_short_name_box($s1→short_name));
    }
  }
  meta private;
}

action show_master3_page(short_name1: String) {
  data→level3 := wall→push_new_page;
  wall→set_reversed(true);
  wall→set_title($short_name1);
  foreach s in records→species_index where $s→short_name→get→equals($short_name1) do {
    code→post_short_bird_info($s);
  }
  meta private;
}
var level3 : Page {
}

action post_short_bird_info(s1: species) {
  $tb1 := wall→create_text_box($s1→english_name→get, 30);
  $tb1→set_background(colors→background);
  $tb1→set_foreground(colors→foreground);
  $tb1→post_to_wall;
  records→TextBox_decorator→at($tb1)→species_id→set($s1→id);
  $tb1 := wall→create_text_box($s1→latin_name→get, 20);
  $tb1→set_background(colors→background);
  $tb1→set_foreground(colors→subtle);
  $tb1→post_to_wall;
  records→TextBox_decorator→at($tb1)→species_id→set($s1→id);
  meta private;
}
var start_page : Page {
}

action main() {
  // When app is started, show the main screen.
  data→start_page := wall→current_page;
  wall→set_reversed(true);
  wall→set_title("MY BIRD LOG");
  $x := 1.43;
  $pic := art→cardinal→clone;
  $pic→resize(art→cardinal→width * $x, art→cardinal→height * $x);
  $pic→post_to_wall;
  code→post_menu_item("About");
  code→post_menu_item("browse");
  if records→species_index→count = 0 then {
    code→download_species_index;
  }
}

action post_menu_item(s1: String) {
  $tb1 := wall→create_text_box($s1→to_lower_case, 45);
  $tb1→set_border(colors→foreground);
  $tb1→set_background(colors→background);
  $tb1→set_foreground(colors→foreground);
  $tb1→post_to_wall;
  meta private;
}
var cardinal : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?c=VSdbwtdq&w=320&h=480&i=http%3A%2F%2Fwww.kewlwallpapers.com%2Fbulkupload%2F133%2FAnimals%2FCardinal.jpg";
}
table thing {
  type = "Decorator";
  keys {
    target: TextBox
  }
  fields {
    species_id: Number_field
  }
}

action show_detail_page(id: Number) {
  data→detail := wall→push_new_page;
  wall→set_reversed(true);
  $s := records→species_index→at($id);
  data→id := $id;
  wall→add_button("add", "add new");
  wall→set_title($s→english_name→get);
  wall→set_subtitle($s→latin_name→get);
  $tb1 := wall→create_text_box("tap here to bing pictures", 20);
  $tb1→set_background(colors→background);
  $tb1→set_foreground(colors→foreground);
  $tb1→post_to_wall;
  $tb1 := wall→create_text_box("", 20);
  $tb1→set_background(colors→background);
  $tb1→post_to_wall;
  $x := $s→sightings_count→get;
  $tb1 := wall→create_text_box("Sightings:  (" ∥ $x→to_string ∥ ")", 20);
  $tb1→set_background(colors→background);
  $tb1→set_foreground(colors→foreground);
  $tb1→post_to_wall;
  code→show_sightings_list($id);
  if $x > 0 then {
    $tb1 := wall→create_text_box("tap here to see map", 20);
    $tb1→set_background(colors→foreground);
    $tb1→set_foreground(colors→background);
    $tb1→post_to_wall;
  }
  meta private;
}
var detail : Page {
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("add new") then {
    code→new_sighting(data→id);
    wall→pop_page;
    code→show_detail_page(data→id);
  }
  else {
    code→show_sightings_on_map(data→id);
  }
  meta private;
}
var id : Number {
}

action contrast(bgcolor: Color) returns fgcolor: Color {
  skip;
  if $bgcolor→brightness > .5 then {
    $fgcolor := colors→black;
  }
  else {
    $fgcolor := colors→white;
  }
  meta private;
}

action show_about_page() {
  wall→push_new_page;
  wall→set_reversed(true);
  wall→set_title("About");
  $tb1 := wall→create_text_box("Keep track of your bird sightings with this little TouchDevelop script!\n\nTo get started, tap \"Download\" to load an index of about 1000 North American bird species. You only need to do this once.\n\nNext, hit \"Browse\" to select a bird species. Choose the initial letter, choose the general bird name, then choose the full bird name.\n\nOn the species page, you can\n1) bing pictures.\n2) record a sighting (hit the \"plus\" button). This automatically captures the current time and location. \n3) display your sightings on a map.\n\n\n\n", 22);
  $tb1→post_to_wall;
  meta private;
}

action show_pics_page() {
  data→pics_page := wall→push_new_page;
  wall→set_reversed(true);
  $s := records→species_index→at(data→id);
  $links := web→search_images($s→latin_name→get);
  $links→post_to_wall;
  meta private;
}
var pics_page : Page {
}

