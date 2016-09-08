// (none)
meta version "v2.2,nothing";
meta name "For each picture";
//
meta platform "media";

action main() {
  foreach pic in collections->create_location_collection() where true do {
    //:: ExpectedOutput(unreachable)
    $pic->post_to_wall();
  }
}
