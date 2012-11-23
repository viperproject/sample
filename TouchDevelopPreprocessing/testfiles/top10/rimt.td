meta version "v2.2,nothing";
meta name "Free Music! • ex.fm stream";
meta icon "play";
meta color "#ffff7518";
// This app uses the online service ex.fm to stream music for free.
//
// Please do not modify this to download music, ex.fm is for streaming only!
meta platform "current";

action Free_Music() {
  wall→clear;
  wall→set_background(colors→gray);
  wall→set_foreground(colors→black);
  $search := wall→ask_string("Search song");
  wall→clear;
  "Please wait..." →post_to_wall;
  $search := $search→replace("-", "");
  $search := $search→replace(",", "");
  $search := $search→replace("  ", " ");
  $json := web→download_json("http://ex.fm/api/v3/song/search/" ∥ web→url_encode($search) ∥ "?page=2");
  if $json→is_invalid then {
    wall→prompt("Could not get songs, maybe no connection?");
    code→Free_Music;
  }
  $r := $json→field("results");
  $x := 0;
  if $r→is_invalid then {
    $x := 0;
  }
  else {
    $x := $r→to_number;
  }
  if $x ≤ 0 then {
    wall→clear;
    wall→prompt("No songs found");
    code→Free_Music;
  }
  $d := collections→create_string_collection;
  //
  for 0 ≤ i < $x - 1 do {
    $s := $json→field("songs")→at($i)→string("artist");
    $s1 := $json→field("songs")→at($i)→string("title");
    if $s→is_invalid then {
      $s := "<unknown>";
    }
    if $s1→is_invalid then {
      $s1 := "<unknown>";
    }
    $listitem := $s ∥ " • " ∥ $s1;
    $d→add($listitem);
  }
  wall→clear;
  $x2 := wall→pick_string("", "", $d);
  wall→clear;
  $songauthor := $json→field("songs")→at($x2)→string("artist");
  if $songauthor→is_invalid then {
    $songauthor := "<unknown>";
  }
  $songalbum := $json→field("songs")→at($x2)→string("album");
  if $songalbum→is_invalid then {
    $songalbum := "<unknown>";
  }
  $songtitle := $json→field("songs")→at($x2)→string("title");
  if $songtitle→is_invalid then {
    $songtitle := "<unknown>";
  }
  $songimgurl := $json→field("songs")→at($x2)→field("image")→string("large");
  $songurl := $json→field("songs")→at($x2)→string("url");
  if not $songurl→is_invalid then {
    data→song := web→download_song($songurl, $songtitle);
    wall→add_button("feature.search", "Song");
    wall→add_button("transport.play", "Play");
    wall→add_button("Transport.pause", "Pause");
  }
  data→buylink := $json→field("songs")→at($x2)→string("buy_link");
  if not data→buylink→is_invalid then {
    wall→add_button("download", "Buy song");
  }
  if not $songimgurl→is_invalid then {
    $pic := web→download_picture($songimgurl);
    if $pic→is_invalid then {
      skip;
    }
    else {
      $pic→post_to_wall;
    }
  }
  ("Title: " ∥ $songtitle)→post_to_wall;
  ("Album: " ∥ $songalbum)→post_to_wall;
  ("Author: " ∥ $songauthor)→post_to_wall;
  web→link_url("", $songurl);
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("Play") then {
    if player→is_paused and player→active_song→name→equals(data→song→name) then {
      player→resume;
    }
    else {
      player→play(data→song);
    }
  }
  if $item→text→equals("Pause") then {
    player→pause;
  }
  if $item→icon→equals("download") then {
    web→browse(data→buylink);
  }
  if $item→icon→equals("feature.search") then {
    code→Free_Music;
  }
  meta private;
}
var song : Song {
}
var buylink : String {
}

action ex\u002Efm_terms_of_service() {
  web→browse("http://ex.fm/terms");
  time→stop_and_close;
}

