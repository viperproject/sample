meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "Announce Song";
meta rootId "BQOI8QAi4KPY1NSNLuTgWwnZ";
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

//:: ExpectedOutput(backward.counterexample.found)
event active_song_changed() {
  if not player→active_song→is_invalid then {
    $sound := languages→speak("en", "Now playing " ∥ player→active_song→name ∥ " by " ∥ player→active_song→artist);
    //:: ExpectedOutput(assert.failed)
    $sound→post_to_wall;
    $sound→play;
  }
  meta private;
}
