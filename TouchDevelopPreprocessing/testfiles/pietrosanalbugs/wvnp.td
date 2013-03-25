meta version "v2.2,nothing";
meta name "quran online player";
meta icon "moon";
meta color "#ff008000";
// thanks to islamway.com
// warning : uses exclusively web contents, 3G charges may apply, use wifi instead.
meta platform "current";

action entryPoint() {
  records→surah_index→clear;
  if records→surah_index→count = 0 then {
    for 0 ≤ i < 114 do {
      $number := code→Pad($i + 1, 3);
      records→surah_index→at($number)→url→set(code→GetUrl($i + 1));
      records→surah_index→at($number)→box→set(code→CreateBox(records→surah_index→at($number)));
    }
  }
  wall→set_reversed(true);
  foreach surah1 in records→surah_index where true do {
    $surah1→box→get→post_to_wall;
  }
}
var baseUrl : String {
  // http://download.quran.islamway.net/quran3/82/036.mp3
  // اللهم تغمد أبي برحمتك
}
table surah {
  type = "Index";
  keys {
    number: String
  }
  fields {
    url: String_field
    box: TextBox_field
  }
}

action GetUrl(n: Number) returns url: String {
  $baseurl := "http://download.quran.islamway.net/quran3/82/";
  $url := $baseurl ∥ code→Pad($n, 3) ∥ ".mp3";
  meta private;
}

action Pad(number: Number, len: Number) returns padded: String {
  $str := $number→to_string;
  if $str→count < $len then {
    for 0 ≤ i < $len - $str→count do {
      $str := "0" ∥ $str;
    }
  }
  $padded := $str;
  meta private;
}

action CreateBox(surah1: surah) returns box1: TextBox {
  $box1 := wall→create_text_box($surah1→number ∥ "\n" ∥ $surah1→url→get, 24);
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  $sid := $item→text→split("\n")→at(0);
  $url := records→surah_index→at($sid)→url→get;
  web→play_media($url);
  meta private;
}

