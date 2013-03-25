meta version "v2.2,nothing";
meta name "ctelth2";
//
meta platform "current";

action main() {
  $s := wall→ask_boolean("Continue?", "Do you want to go to Google Voice?");
  if $s→equals(true) then {
    web→browse("https://voice.google.com");
  }
}

