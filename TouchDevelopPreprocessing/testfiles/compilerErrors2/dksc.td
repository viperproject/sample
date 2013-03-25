meta version "v2.2,nothing";
meta name "spelling phone";
meta icon "sound";
meta color "#ff008000";
// Type a word in english and
// the app will spell the word according
// to the ICAO spelling standart.
meta platform "current";

action main() {
  wall→set_reversed(true);
  wall→set_background(colors→chrome→make_transparent(0.2));
  code→init_Sound;
  wall→add_button("upload", "");
  code→post_menu;
}
table ICAO_Alphabet {
  type = "Index";
  keys {
    alpha_numerics_: String
  }
  fields {
    sound: Sound_field
  }
}

action ICAO_strings() {
  // http://de.m.wikipedia.org/wiki/Buchstabiertafel#section_2
  //
  $ICAOs := collections→create_string_collection;
  // numbers
  $ICAOs→add("Zero");
  $ICAOs→add("One");
  $ICAOs→add("Two");
  $ICAOs→add("Three");
  $ICAOs→add("Four");
  $ICAOs→add("Five");
  $ICAOs→add("Six");
  $ICAOs→add("Seven");
  $ICAOs→add("Eight");
  $ICAOs→add("Nine");
  //
  $ICAOs→add("Alfa");
  $ICAOs→add("Bravo");
  $ICAOs→add("Charlie");
  $ICAOs→add("Delta");
  $ICAOs→add("Echo");
  $ICAOs→add("Foxtrot");
  $ICAOs→add("Golf");
  $ICAOs→add("Hotel");
  $ICAOs→add("India");
  $ICAOs→add("Juliett");
  $ICAOs→add("Kilo");
  $ICAOs→add("Lima");
  $ICAOs→add("Mike");
  $ICAOs→add("November");
  $ICAOs→add("Oscar");
  $ICAOs→add("Papa");
  $ICAOs→add("Quebec");
  $ICAOs→add("Romeo");
  $ICAOs→add("Sierra");
  $ICAOs→add("Tango");
  $ICAOs→add("Uniform");
  $ICAOs→add("Victor");
  $ICAOs→add("Whiskey");
  $ICAOs→add("X-ray");
  $ICAOs→add("Yankee");
  $ICAOs→add("Zulu");
  meta private;
}

action create_ICAO_Map() {
  // http://de.m.wikipedia.org/wiki/Buchstabiertafel#section_2
  if data→ICAO_Alpha_Map→is_invalid then {
    data→ICAO_Alpha_Map := collections→create_string_map;
    data→ICAO_Alpha_Map→set_at("0", "Zero");
    data→ICAO_Alpha_Map→set_at("1", "One");
    data→ICAO_Alpha_Map→set_at("2", "Two");
    data→ICAO_Alpha_Map→set_at("3", "Three");
    data→ICAO_Alpha_Map→set_at("4", "Four");
    data→ICAO_Alpha_Map→set_at("5", "Five");
    data→ICAO_Alpha_Map→set_at("6", "Six");
    data→ICAO_Alpha_Map→set_at("7", "Seven");
    data→ICAO_Alpha_Map→set_at("8", "Eight");
    data→ICAO_Alpha_Map→set_at("9", "Nine");
    //
    data→ICAO_Alpha_Map→set_at("A", "Alfa");
    data→ICAO_Alpha_Map→set_at("B", "Bravo");
    data→ICAO_Alpha_Map→set_at("C", "Charlie");
    data→ICAO_Alpha_Map→set_at("D", "Delta");
    data→ICAO_Alpha_Map→set_at("E", "Echo");
    data→ICAO_Alpha_Map→set_at("F", "Foxtrot");
    data→ICAO_Alpha_Map→set_at("G", "Golf");
    data→ICAO_Alpha_Map→set_at("H", "Hotel");
    data→ICAO_Alpha_Map→set_at("I", "India");
    data→ICAO_Alpha_Map→set_at("J", "Juliett");
    data→ICAO_Alpha_Map→set_at("K", "Kilo");
    data→ICAO_Alpha_Map→set_at("L", "Lima");
    data→ICAO_Alpha_Map→set_at("M", "Mike");
    data→ICAO_Alpha_Map→set_at("N", "November");
    data→ICAO_Alpha_Map→set_at("O", "Oscar");
    data→ICAO_Alpha_Map→set_at("P", "Papa");
    data→ICAO_Alpha_Map→set_at("Q", "Quebec");
    data→ICAO_Alpha_Map→set_at("R", "Romeo");
    data→ICAO_Alpha_Map→set_at("S", "Sierra");
    data→ICAO_Alpha_Map→set_at("T", "Tango");
    data→ICAO_Alpha_Map→set_at("U", "Uniform");
    data→ICAO_Alpha_Map→set_at("V", "Victor");
    data→ICAO_Alpha_Map→set_at("W", "Whiskey");
    data→ICAO_Alpha_Map→set_at("X", "X-ray");
    data→ICAO_Alpha_Map→set_at("Y", "Yankee");
    data→ICAO_Alpha_Map→set_at("Z", "Zulu");
    $ICAO_Map := data→ICAO_Alpha_Map;
    //
  }
  meta private;
}

action init_Sound() {
  if records→ICAO_Alphabet_index→count < 36 then {
    code→create_ICAO_Map();
    foreach s in data→ICAO_Alpha_Map where true do {
      $sound := languages→speak("en", data→ICAO_Alpha_Map→at($s));
      records→ICAO_Alphabet_index→at($s)→sound→set($sound);
    }
  }
  meta private;
}
var ICAO_Alpha_Map : String_Map {
}

event tap_wall_ICAO_Alphabet(item: ICAO_Alphabet) {
  $item→sound→get→play;
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  if $item→text→starts_with("\n⎈ Post") then {
    wall→clear;
    $tb := wall→create_text_box("Tap ICAO item to play sound", 28);
    $tb→set_background(colors→blue);
    $tb→post_to_wall;
    foreach s in data→ICAO_Alpha_Map→keys where true do {
      records→ICAO_Alphabet_index→at($s)→post_to_wall;
    }
  }
  if $item→text→starts_with("\n⌨ WORD") then {
    wall→clear;
    $s1 := wall→ask_string("Please enter one Word to spell:");
    $s1 := $s1→to_upper_case;
    languages→speak("en", "spelling ")→play;
    time→sleep(1);
    languages→speak("en", $s1)→play;
    time→sleep(4);
    foreach s2 in $s1 where true do {
      $s3 := $s2 ∥ ": " ∥ data→ICAO_Alpha_Map→at($s2);
      $s3→post_to_wall;
      records→ICAO_Alphabet_index→at($s2)→sound→get→play;
      time→sleep(1.8);
    }
    time→sleep(3);
    code→post_menu;
  }
  if $item→text→starts_with("\n◎ Speak") then {
    foreach ia in records→ICAO_Alphabet_index where true do {
      $ia→sound→get→play;
      time→sleep(1.5);
    }
  }
  meta private;
}

action post_menu() {
  wall→clear;
  wall→set_background(colors→chrome→darken(0.02));
  $tb1 := wall→create_text_box("\n⌨ WORD ICAO spelling\n", 36);
  $tb1→set_foreground(colors→accent);
  $tb1→post_to_wall;
  $tb := wall→create_text_box("\n⎈ Post ICAO Alphabet\n", 36);
  $tb→set_foreground(colors→accent);
  $tb→post_to_wall;
  $tb2 := wall→create_text_box("\n◎ Speak ICAO Alphabet\n", 36);
  $tb2→set_foreground(colors→accent);
  $tb2→post_to_wall;
  meta private;
}

