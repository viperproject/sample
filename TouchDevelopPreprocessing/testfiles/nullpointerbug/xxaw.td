meta version "v2.2,nothing";
meta name "my player";
meta icon "mp3player";
meta color "#ffff0038";
//
meta platform "current,accelerometer,gyroscope,home,motion,phone";

action main() {
  code→index;
}

event tap_wall_Page_Button(item: Page_Button) {
  $s := $item→text;
  $b := true;
  code→ontab($s, $b);
  meta private;
}
var play\u002Fpause : Page_Button {
}
var edit : Page_Button {
}

event tap_wall_String(item: String) {
  if($item→equals("Songs")) then {
    code→songs;
  }
  if $item→equals("Playlist") then {
    code→playlist;
  }
  if $item→equals("Songs album") then {
    code→album;
  }
  meta private;
}

action album() {
  data→page := wall→push_new_page;
  data→pageno := "Album";
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Album");
  media→song_albums→post_to_wall;
  tags;
  code→wallbutton;
  meta private;
}

action songs() {
  data→page := wall→push_new_page;
  data→pageno := "Song";
  wall→set_title("Songs");
  wall→clear_buttons;
  $songs := media→songs;
  $songs→post_to_wall;
  ... ;
  code→wallbutton;
  meta private;
}

action playlist() {
  data→page := wall→push_new_page;
  data→pageno := "Playlist";
  wall→clear;
  wall→set_title("Playlist");
  wall→clear_buttons;
  media→playlists→post_to_wall;
  code→wallbutton;
  meta private;
}

event tap_wall_Song_Album(item: Song_Album) {
  data→pageno := "Albumsong";
  wall→push_new_page;
  wall→clear;
  wall→clear_buttons;
  wall→set_reversed(true);
  wall→set_title($item→name);
  $item→art→post_to_wall;
  $item→songs→post_to_wall;
  data→songcoll := $item→songs;
  code→wallbutton;
  meta private;
}

event tap_wall_Song(item: Song) {
  wall→push_new_page;
  data→pageno := "Songplay";
  wall→clear;
  wall→clear_buttons;
  wall→set_reversed(true);
  wall→set_title($item→name);
  $pic := $item→album→art;
  $pic→resize(200, - 100);
  $pic→post_to_wall;
  $item→album→name→post_to_wall;
  $item→artist→post_to_wall;
  $item→genre→post_to_wall;
  $x := $item→duration;
  $x := $x / 60;
  $x := math→round_with_precision($x, 2);
  $x→post_to_wall;
  player→play($item);
  ... ;
  code→wallbutton;
  meta private;
}
var menu : Tile {
  readonly = true;
}
var setting : Page_Button {
}

action wallbutton() {
  wall→clear_buttons;
  if player→is_playing then {
    data→play\u002Fpause := wall→add_button("transport.pause", "Pause");
  }
  else {
    data→play\u002Fpause := wall→add_button("transport.play", "Play");
  }
  meta private;
}

action index() {
  wall→clear_buttons;
  wall→set_title("Songs");
  wall→set_subtitle("Select option");
  wall→set_reversed(true);
  data→pageno := "Index";
  data→menu→set_title("My player");
  data→pageno := "Home";
  "\n\n" →post_to_wall;
  "Songs" →post_to_wall;
  "\n\n" →post_to_wall;
  "Songs album" →post_to_wall;
  "\n\n" →post_to_wall;
  "Playlist" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  "\n\n" →post_to_wall;
  ... ;
  "\n\n" →post_to_wall;
  wall→create_text_box("Instruction", 19)→post_to_wall;
  wall→create_text_box("Advance setting", 19)→post_to_wall;
  code→wallbutton;
  meta private;
}
var song : Song {
}

action ontab(item: String, b: Boolean) {
  ... ;
  $s := data→edit→text;
  $s1 := data→play\u002Fpause→text;
  if $item→equals($s) and $b then {
    data→page := wall→push_new_page;
    wall→set_title("Help");
    "\n\n\n" →post_to_wall;
    "             🎷shake to shuffle song🎸" →post_to_wall;
    "\n\n\n\n" →post_to_wall;
    $b := false;
  }
  if $item→equals($s1) and $b then {
    if player→is_paused and $b then {
      player→active_song→play;
      wall→clear_buttons;
      code→wallbutton;
      $b := false;
    }
    if player→is_playing and $b then {
      player→pause;
      wall→clear_buttons;
      code→wallbutton;
      $b := false;
    }
  }
  meta private;
}
var playlist2 : Playlist {
}
var songcoll : Songs {
}

action collection(s1: String) {
  ... ;
  meta private;
}
var page : Page {
}
var pageno : String {
}

event page_navigated_from() {
  code→wallbutton;
  meta private;
}

action go() {
  if player→is_playing then {
    data→play\u002Fpause := wall→add_button("transport.pause", "Pause");
    data→edit := wall→add_button("edit", "Help");
    data→setting := wall→add_button("feature.settings", "Setting");
  }
  else {
    data→play\u002Fpause := wall→add_button("transport.play", "Play");
    data→edit := wall→add_button("edit", "Help");
    data→setting := wall→add_button("feature.settings", "Setting");
  }
  meta private;
}

event shake() {
  media→songs→random→play;
  code→wallbutton;
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  $s := "Instruction";
  ... ;
  $s1 := "Advance setting";
  if $item→text→equals($s1) then {
    skip;
  }
  if $item→text→equals($s) then {
    data→page := wall→push_new_page;
    wall→set_title("Help");
    "\n\n\n" →post_to_wall;
    "             🎷shake to shuffle song🎸" →post_to_wall;
    "\n\n\n\n" →post_to_wall;
  }
  meta private;
}

