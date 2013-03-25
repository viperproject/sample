meta version "v2.2,nothing";
meta name "my string reverser";
//
meta platform "current";

action main() {
  wall→clear;
  wall→set_reversed(true);
  wall→ask_string("How many copies of Star Trek do you want?")→post_to_wall;
  $mainString := "Hello world!";
  $mainString→post_to_wall;
  foreach e in $mainString where true do {
    $e→to_upper_case→post_to_wall;
  }
}

