meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "Where the Hell Am I?";
meta icon "gps";
meta color "#ffff0038";
meta rootId "xdZZVkgQ0TuehejuU2RRNRTs";
meta platform "current";
// Pops up a Bing map centered on your phone's current location.
//:: ExpectedOutput(backward.counterexample.found)
action main() {
  "Running script..." →post_to_wall;
  $loc := senses→current_location;
  //:: ExpectedOutput(assert.failed)
  $loc→post_to_wall;
}

var time2 : DateTime {
  // time
}

var c : Color {
  is\_resource = true;
  url = "#ffbc3e3e";
  transient = true;
}
