meta version "v2.2,nothing";
meta name "ColorSwapper";
meta icon "Cycle";
meta color "#ffA52A2A";
//
meta platform "accelerometer,camera,compass,location,maps,media,contacts,orientation,search,translation,musicandsounds,webonly";

action main() {
  $pic2 := media→choose_picture;
  $rows := $pic2→height;
  $cols := $pic2→width;
  for 0 ≤ i < $rows do {
    for 0 ≤ j < $cols do {
      $c := $pic2→pixel($j, $i);
      $c2 := colors→from_rgb($c→B, $c→G, $c→R);
      $pic2→set_pixel($j, $i, $c2);
    }
  }
  $pic2→post_to_wall;
}
var pic : Picture {
  is\_resource = true;
}

