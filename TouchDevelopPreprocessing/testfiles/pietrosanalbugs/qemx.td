meta version "v2.2,nothing";
meta name "plohh";
//
meta platform "current";
meta import data_seattle_gov {
  pub "ddhb"
  usage {
    action emergency_911_calls(loc: Location, miles: Number, start: Number, count: Number) returns calls: Message_Collection
    action traffic_cameras(location: Location, miles: Number, start: Number, count: Number) returns places: Place_Collection
  }
}

action main() {
  $loc := senses→current_location;
  $msgs := ♻ data_seattle_gov→emergency_911_calls($loc, 3, 0, 10);
  $msgs→post_to_wall;
  $places := ♻ data_seattle_gov→traffic_cameras($loc, 3, 0, 10);
  foreach place in $places where true do {
    web→link_image($place→picture_link)→post_to_wall;
  }
}

