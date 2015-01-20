meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "internet speedtest";
meta icon "mobilephone";
meta rootId "RjfjEch2fi67AqE1ZRSZluXI";
meta platform "current";
meta parentIds "";
// Test your download speed.
// There will 684kb downloaded make sure if are mobile internet you have data plan.
// I'm not responsible for costs this app generate.
// With leader board ;)
//
// V 0.1 Beta
#main
action main() {
  $x := code→extracted;
  $x1 := math→round_with_precision(684 / $x / 1024, 4);
  $s := web→connection_type;
  $s1 := "You have a speed of " ∥ $x1 ∥ " mb\\s on " ∥ $s;
  if $s→equals("mobile") then {
    $x1 := $x1 - $x1;
  }
  bazaar→post_leaderboard_score($x1);
  "Leader board only for WiFi tests " →post_to_wall;
  bazaar→post_leaderboard_to_wall;
  $s1→post_to_wall;
}
#extracted
action extracted() returns(x: Number) {
  $x1 := 0;
  for 0 ≤ i < 1 do {
    $dt := time→now;
    $pic := web→download_picture("http://25.media.tumblr.com/tumblr_mb9qm489K91rol3amo1_400.jpg");
    $dt1 := time→now;
    skip;
    $x1 := $x1 + $dt1→subtract($dt);
  }
  skip;
  $x := $x1;
  meta private;
}