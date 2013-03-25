meta version "v2.2,nothing";
meta name "Radio v1";
meta icon "Music";
meta color "#ffff0038";
// An app that uses the radio module in the phone to stream music.
//
// Version 1.0
meta platform "current";

action main() {
  code→setup();
  $x := wall→ask_number("Input wanted radio frequency (MHz).\n\n");
  radio→set_frequency($x);
  wall→clear;
  while true do {
    $x1 := senses→acceleration_quick→z;
    if $x1 > 1 then {
      $b := wall→ask_boolean("Turn off the radio.", "");
      if $b→equals(true) then {
        radio→stop;
      }
    }
  }
  wall→clear;
}

action setup() {
  wall→clear;
  radio→start;
  wall→prompt("Plug in headphones they serve as an antenna. Swat the phone down to stop playing the radio.");
  ... ;
  meta private;
}

