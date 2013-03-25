meta version "v2.2,nothing";
meta name "Unplayed Songs";
meta icon "mute";
meta color "#ffff7518";
//
meta platform "current";

action main() {
  // Finds songs not played yet.
  records→songdata_table→clear;
  $found := 0;
  $total := 0;
  $songs := media→songs;
  foreach song in $songs where true do {
    $total := $total + 1;
    $found := $found + code→display_song($song);
  }
  ("Songs played with this script:  " ∥ data→played)→post_to_wall;
  ("Songs never played:  " ∥ $found)→post_to_wall;
  ("Total count of song in songs:  " ∥ $total)→post_to_wall;
}

action display_song(song: Song) returns result: Number {
  // Post a song to the wall if not played yet and return 1;
  // Otherwise return 0
  if $song→play_count = 0 then {
    // jhm song -> post to wall
    $result := 1;
  }
  else {
    $result := 0;
  }
  $song→post_to_wall;
  $stitle := $song→name;
  $salbum := $song→album→name;
  $sartist := $song→artist;
  $rsong := records→songdata_table→add_row;
  $rsong→title→set($stitle);
  $rsong→album→set($salbum);
  $rsong→artist→set($sartist);
  $keysong := records→songkeys_index→at($sartist);
  $keysong→album→set($salbum);
  $keysong→title→set($stitle);
  meta private;
}

event active_song_changed() {
  // Increment the song played counter.
  data→played := data→played + 1;
  meta private;
}
var played : Number {
}

event shake() {
  // Pauses and resumes play.
  if player→is_playing then {
    player→pause;
  }
  else {
    player→resume;
  }
  meta private;
}
table songdata {
  // contains data about songs found
  type = "Table";
  fields {
    title: String_field
    album: String_field
    artist: String_field
  }
}
table songkeys {
  type = "Index";
  keys {
    artist: String
  }
  fields {
    album: String_field
    title: String_field
  }
}

