meta version "v2.2,nothing";
meta name "musicSay";
meta icon "smsalt";
//
meta platform "current,media";

action main() {
  skip;
}

event active_song_changed() {
  $music := player→active_song→name;
  $musicName := languages→speak("en", $music);
  $artist := player→active_song→artist;
  $artistName := languages→speak("en", $artist);
  if not $musicName→is_invalid then {
    player→pause;
    $x := $musicName→volume;
    $musicName→set_volume(1);
    $musicName→play;
    time→sleep($musicName→duration);
    $artistName→play;
    time→sleep($artistName→duration);
    $musicName→set_volume($x);
    $s := records→speakTab_table→add_row;
    $s→songName→set($music);
    $s→artistName→set($artist);
    $s→speakSong→set($musicName);
    $s→speakArtist→set($artistName);
    ... ;
  }
  player→resume;
  player→active_song→post_to_wall;
  meta private;
}
table speakTab {
  type = "Table";
  fields {
    songName: String_field
    artistName: String_field
    speakSong: Sound_field
    speakArtist: Sound_field
  }
}

