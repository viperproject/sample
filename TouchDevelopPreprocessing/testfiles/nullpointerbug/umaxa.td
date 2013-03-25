meta version "v2.2,nothing";
meta name "countmysongs";
meta icon "music";
meta color "#ff00008b";
// version 0.1
// next version will include a tile and maybe a leaderbord :)
// have fun!
meta platform "current";

action main() {
  code→init;
  code→print_data;
  code→print_refresh_button;
}
var songs_count : Number {
}

action init() {
  if data→songs_count→is_invalid or data→songs_count = 0 then {
    data→songs_count := media→songs→count;
    data→diff := data→songs_count;
  }
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→icon→equals("refresh") then {
    code→count_\u002B_diff;
    wall→clear;
    code→print_data;
  }
  meta private;
}

action print_data() {
  $fontsizebig := 80;
  $fontsizesmall := 20;
  $bgcolor := colors→accent;
  $first_tb := wall→create_text_box("your library contains", $fontsizesmall);
  $songs_count_tb := wall→create_text_box(data→songs_count→to_string, $fontsizebig);
  $s := "removed";
  if data→diff > - 1 then {
    $s := "added";
  }
  $second_tb := wall→create_text_box("song(s).\n\n\nsince the last counter refresh, you " ∥ $s, $fontsizesmall);
  $diffabs := math→abs(data→diff);
  $diff_tb := wall→create_text_box($diffabs→to_string, $fontsizebig);
  $s := "from";
  if data→diff > - 1 then {
    $s := "to";
  }
  $third_tb := wall→create_text_box("song(s) " ∥ $s ∥ " your library.", $fontsizesmall);
  $first_tb→set_background($bgcolor);
  $songs_count_tb→set_background($bgcolor);
  $second_tb→set_background($bgcolor);
  $diff_tb→set_background($bgcolor);
  $third_tb→set_background($bgcolor);
  $songs_count_tb→set_icon(media→large_icon("music"));
  $s := "trash";
  if data→diff > - 1 then {
    $s := "addcircle";
  }
  $diff_tb→set_icon(media→large_icon($s));
  wall→set_background($bgcolor);
  wall→set_reversed(true);
  $first_tb→post_to_wall;
  $songs_count_tb→post_to_wall;
  $second_tb→post_to_wall;
  $diff_tb→post_to_wall;
  $third_tb→post_to_wall;
  meta private;
}
var diff : Number {
}

action count_\u002B_diff() {
  $songs_count_new := media→songs→count;
  if $songs_count_new ≠ data→songs_count then {
    data→diff := $songs_count_new - data→songs_count;
    data→songs_count := $songs_count_new;
  }
}

action print_refresh_button() {
  wall→add_button("refresh", "refresh counter");
  meta private;
}

action reset_counter_\u002B_diff() {
  data→songs_count := 0;
  data→diff := 0;
}

