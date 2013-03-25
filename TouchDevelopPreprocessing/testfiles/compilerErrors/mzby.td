meta version "v2.2,nothing";
meta name "songname it";
meta icon "sms";
meta color "#ff004b49";
//
meta platform "current";

action main() {
  data→counter := 10;
  for 0 ≤ i < data→counter do {
    $song := media→songs→random;
    ... ;
  }
}
var counter : Number {
}
table songsTable {
  type = "Object";
  fields {
    songs: Songs_field
  }
}
table musicVoice {
  type = "Index";
  keys {
    name: String
  }
  fields {
    sound: Sound_field
  }
}
table artistVoice {
  type = "Index";
  keys {
    name: String
  }
  fields {
    sound: Sound_field
  }
}

event active_song_changed() {
  $isPlaying := player→is_playing;
  $music := code→getMusic();
  $artist := code→getArtist();
  if $isPlaying then {
    player→pause;
  }
  $artist→play;
  time→sleep(1.5);
  $music→play;
  time→sleep(1.5);
  if $isPlaying then {
    player→resume;
  }
  meta private;
}

action getSound(s: String) returns sound: Sound {
  $lang := languages→detect_language($s);
  $sound := languages→speak($lang, $s);
  meta private;
}

action getArtist() returns artistSound: Sound {
  $artist := player→active_song→artist;
  $resultArtist := records→artistVoice_index→at($artist);
  if $resultArtist→sound→get→is_invalid then {
    $resultArtist→sound→set(code→getSound($artist));
  }
  $artistSound := $resultArtist→sound→get;
  meta private;
}

action getMusic() returns musicSound: Sound {
  $music := player→active_song→name;
  $resultMusic := records→artistVoice_index→at($music);
  if $resultMusic→sound→get→is_invalid then {
    $resultMusic→sound→set(code→getSound($music));
  }
  $musicSound := $resultMusic→sound→get;
  meta private;
}

