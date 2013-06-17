// (none)
meta version "v2.2,nothing";
meta name "For each picture";
//
meta platform "media";

action main() {
  foreach pic in media→pictures where true do {
    $pic→resize(64,64);
    $pic→post_to_wall;
    $pic→date→post_to_wall;
  }
}
