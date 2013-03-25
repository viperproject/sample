meta version "v2.2,nothing";
meta name "my phone";
// You can use your phone without lenses
// Listen to your favourite radio station
//
// With Vodafone sim you can send SMS to check the available data balance
// You can see the day date and time
// You can check the direction with number
// You are informed whether you are connected with internet
// You can search for the contacts
//
// Contact number can be heard
//
// And everything with big display
// Go and proceed
meta platform "current";

action vdfone_databalance() {
  ... ;
  social→send_sms("111", "Databal");
  ... ;
  meta private;
}

action start_() {
  ... ;
  data→v54 := 54;
  wall→display_search(false);
  senses→camera→post_to_wall;
  $radio1 := wall→create_text_box("", data→v54);
  $radio1→set_text("Fm Radio");
  $radio1→post_to_wall;
  ... ;
  $tb1 := wall→create_text_box("Search", 54);
  $tb1→post_to_wall;
  ... ;
  $tb2 := wall→create_text_box("Current data", 54);
  $tb2→post_to_wall;
  ... ;
  ... ;
  $tb := wall→create_text_box("", 54);
  $tb→set_text("Data balance");
  $tb→post_to_wall;
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  wall→clear;
  ... ;
  $s := $item→text;
  ... ;
  $s→post_to_wall;
  ... ;
  if $s→equals("Fm Radio") then {
    code→radio_action;
    ... ;
  }
  if $s→equals("Current data") then {
    code→current_data;
    ... ;
  }
  if $s→equals("Search") then {
    code→search_action;
    ... ;
  }
  if $s→equals("Data balance") then {
    ... ;
    code→vdfone_databalance;
  }
  meta private;
}

action current_data() {
  ... ;
  $x := time→now→week_day;
  $s3 := code→sunday($x);
  $s3 := $s3 ∥ time→now→to_string;
  if web→is_connected then {
    $s2 := web→connection_name;
    $tb2 := wall→create_text_box($s2, data→v54);
    $tb2→post_to_wall;
    ... ;
  }
  else {
    "No internet " →post_to_wall;
  }
  $s1 := code→show_news();
  $tb1 := wall→create_text_box($s1, data→v54);
  $tb1→post_to_wall;
  ... ;
  ... ;
  ... ;
  $tb := wall→create_text_box($s3, data→v54);
  $tb→post_to_wall;
  ... ;
  meta private;
}
var search_for : String {
}

action search_action() {
  ... ;
  data→search_for := wall→ask_string("Search for ");
  $contacts := social→search_contacts(data→search_for);
  $x := $contacts→count;
  $s := $x ∥ "  records found ";
  $s→post_to_wall;
  ... ;
  ... ;
  ... ;
  if $x > 0 then {
    wall→display_search(true);
    wall→clear;
    ... ;
    $contacts→post_to_wall;
    $s→post_to_wall;
    ... ;
  }
  meta private;
}

action show_news() returns rnews: String {
  $x := senses→heading;
  $x := math→round($x);
  ... ;
  ... ;
  data→vnews := "";
  ... ;
  if $x > 338 and $x ≤ 360 or $x ≤ 23 then {
    data→vnews := "North ";
  }
  ... ;
  if $x > 23 and $x ≤ 68 then {
    data→vnews := "North east ";
  }
  ... ;
  if $x > 68 and $x ≤ 113 then {
    data→vnews := "East ";
  }
  ... ;
  if $x > 113 and $x ≤ 158 then {
    data→vnews := "South east ";
  }
  ... ;
  if $x > 158 and $x ≤ 203 then {
    data→vnews := "South ";
  }
  ... ;
  if $x > 203 and $x ≤ 248 then {
    data→vnews := "South west ";
  }
  ... ;
  if $x > 248 and $x ≤ 293 then {
    data→vnews := "West ";
  }
  ... ;
  if $x > 293 and $x ≤ 338 then {
    data→vnews := "North west ";
  }
  data→vnews := data→vnews ∥ "  " ∥ $x→to_string;
  ... ;
  ... ;
  ... ;
  ... ;
  $rnews := data→vnews;
  ... ;
  data→vnews := "";
  meta private;
}

event tap_wall_Contact(item: Contact) {
  $link := $item→phone_number;
  $s := $link→address;
  data→sph_number := $s;
  $page := wall→push_new_page;
  $page_button := wall→add_button("Back", "Back");
  $page_button1 := wall→add_button("favourites play", "Play");
  ... ;
  $tb := wall→create_text_box($s, data→v54);
  $tb→post_to_wall;
  $tb1 := wall→create_text_box($item→name, data→v54);
  $tb1→post_to_wall;
  $item→mobile_phone→post_to_wall;
  ... ;
  if not $item→picture→is_invalid then {
    $item→picture→post_to_wall;
    ... ;
  }
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  ... ;
  $s := $item→text;
  $s→post_to_wall;
  wall→display_search(false);
  ... ;
  ... ;
  wall→clear;
  ... ;
  if $s→equals("End") then {
    wall→clear;
    time→stop_and_close;
    ... ;
  }
  ... ;
  if $s→equals("Play") then {
    ... ;
    $s1 := code→text_action(data→sph_number);
    ... ;
    $s1→post_to_wall;
    $sound := languages→speak("en", $s1);
    $sound→play;
    ... ;
  }
  ... ;
  ... ;
  if $s→equals("Clear") then {
    wall→clear;
    time→stop_and_close;
    ... ;
  }
  if $s→equals("Start") then {
    wall→clear;
    code→start_;
  }
  if $s→equals("Back") then {
    wall→pop_page;
  }
  ... ;
  if $s→equals("Update") then {
    wall→clear;
    code→current_data;
    ... ;
  }
  ... ;
  meta private;
}

action main() {
  wall→add_button("Back", "End");
  wall→add_button("Refresh", "Update");
  wall→add_button("Add", "Start");
  ... ;
  data→v54 := 54;
  code→start_;
  ... ;
  bazaar;
}
var v54 : Number {
}

action sunday(insunday: Number) returns rsunday: String {
  $s := "";
  $rsunday;
  if $insunday = 0 then {
    $s := "Sun ";
  }
  if $insunday = 1 then {
    $s := "Mon ";
  }
  if $insunday = 2 then {
    $s := "Tue ";
  }
  if $insunday = 3 then {
    $s := "Wed ";
  }
  if $insunday = 4 then {
    $s := "Thu ";
  }
  if $insunday = 5 then {
    $s := "Fri ";
  }
  if $insunday = 6 then {
    $s := "Sat ";
  }
  ... ;
  ... ;
  ... ;
  ... ;
  ... ;
  ... ;
  ... ;
  $rsunday := $s;
  meta private;
}
var vnews : String {
}

action go() {
  ... ;
  ... ;
}

action radio_action() {
  $frquency := wall→ask_number("Frquency");
  if $frquency > 87 and $frquency < 108 then {
    radio→start;
    radio→set_frequency($frquency);
    $s := radio→frequency→to_string ∥ "Fm Radio";
    wall→set_title($s);
  }
  else {
    radio→stop;
    wall→set_title("");
    ... ;
  }
  meta private;
}

action text_action(in_string: String) returns out_text: String {
  ... ;
  $s_ph := $in_string;
  ... ;
  $ph := "";
  ... ;
  foreach s in $s_ph where true do {
    if not $s→to_number→is_invalid then {
      $s1 := code→below_ten($s→to_number);
      $ph := $ph ∥ " " ∥ $s1;
    }
    if $s→equals("+") then {
      $ph := $ph ∥ " plus ";
    }
    ... ;
  }
  ... ;
  $out_text := $ph;
  meta private;
}

action below_ten(in_number: Number) returns out_text: String {
  $s := "";
  ... ;
  if $in_number = 1 then {
    $s := "One";
  }
  if $in_number = 2 then {
    $s := "Two";
  }
  if $in_number = 3 then {
    $s := "Three";
  }
  if $in_number = 4 then {
    $s := "Four";
  }
  if $in_number = 5 then {
    $s := "Five";
  }
  if $in_number = 6 then {
    $s := "Six";
  }
  if $in_number = 7 then {
    $s := "Seven";
  }
  if $in_number = 8 then {
    $s := "Eight";
  }
  if $in_number = 9 then {
    $s := "Nine";
  }
  if $in_number = 0 then {
    $s := "Zero";
  }
  $out_text := $s;
  meta private;
}
var sph_number : String {
}

