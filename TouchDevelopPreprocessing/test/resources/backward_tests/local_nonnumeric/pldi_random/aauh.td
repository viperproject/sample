meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "The Game";
meta icon "star";
meta color "#ffffa500";
meta rootId "kNKj2skMqzo2blKFHJVLa5uj";
meta platform "current";
// Announce that you just lost the game.

//:: ExpectedOutput(backward.counterexample.found)
action main() {
  $s1 := "I lost the game.";
  $s := languages→speak("en", $s1);
  //:: ExpectedOutput(assert.failed)
  $s→play;
  social→send_sms("", $s1);
  social→create_message($s1)→share("social");
}
