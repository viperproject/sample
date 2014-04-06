meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "Pick me a song!";
meta icon "music";
meta color "#ffff0038";
meta rootId "wHyY4E2SzoZ1JywTgNuCyZ4f";
meta platform "current";


//:: ExpectedOutput(backward.counterexample.found)
action main() {
  $song := media→songs→random;
  //:: ExpectedOutput(assert.failed)
  $song→play;
  wall→set_background(colors→black);
  wall→prompt("Shake for a random song");
  player→active_song→post_to_wall;
}

event shake() {
  wall→clear;
  wall→clear_buttons;
  //:: ExpectedOutput(assert.failed)
  media→songs→random→play;
  player→active_song→post_to_wall;
  wall→set_foreground(colors→random→make_transparent(50));
  wall→set_background(colors→black);
  wall→set_subtitle("Don\'t like it? Shake for another.");
  skip;
  meta private;
}
