meta version "v2.2,nothing";
meta name "text my location";
// Send your address and location through messaging
meta platform "current";

action main() {
  // Grabs location and sends it by sms
  // get a phone number and cache it
  while data→link→is_invalid do {
    data→link := phone→choose_phone_number;
  }
  // get the current location
  $loc := senses→current_location_accurate;
  $address := locations→describe_location($loc);
  data→main_tile→set_back_title($address);
  social→send_sms(data→link→address, "we are at " ∥ $address ∥ ", " ∥ $loc→to_string);
}
var main_tile : Tile {
  readonly = true;
}
var link : Link {
}

