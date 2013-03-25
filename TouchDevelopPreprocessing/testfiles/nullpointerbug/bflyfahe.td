meta version "v2.2,nothing";
meta name "SendLocation";
meta color "#ffff0038";
//
meta platform "current";

action main() {
  $loc := senses→current_location;
  $link := social→choose_contact→mobile_phone;
  $b := wall→ask_boolean("Send Location to ", "");
  if $b→equals(true) then {
    skip;
  }
}

