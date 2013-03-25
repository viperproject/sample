meta version "v2.2,nothing";
meta name "E-Democracy";
//
meta platform "current";

action main() {
  wall→set_title("Will Labor win this time?");
  $b := wall→ask_boolean("", "");
  if $b→equals(false) then {
    "Good Bye " →post_to_wall;
  }
  else {
    "Welcome to labor " →post_to_wall;
    skip;
  }
}
table Question {
  type = "Object";
}

