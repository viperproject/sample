meta version "v2.2,nothing";
meta name "testColors";
//
meta platform "current";

action main() {
  $c := colors→red;
  code→printColor($c);
  $c := $c→make_transparent(.5);
  code→printColor($c);
  $pic := media→create_picture(100, 100);
  $pic→fill_rect(0, 0, 100, 100, 0, colors→red);
  for 0 ≤ i < 10 do {
    $pic→draw_line(0, $i * 10, 100, $i * 10, colors→yellow, 3);
    $c1 := $pic→pixel($i, $i);
    $c2 := $c1→make_transparent(0.1);
    $pic→set_pixel($i, $i, $c2);
  }
  $pic→post_to_wall;
  $c3 := $pic→pixel(5, 5);
  code→printColor($c3);
  $pic1 := media→create_picture(200, 200);
  $pic1→fill_rect(0, 0, 100, 100, 0, colors→blue);
  $c3 := $pic1→pixel(55, 55);
  code→printColor($c3);
  $pic1→blend($pic, 50, 50, 0, .5);
  $pic1→post_to_wall;
  $c3 := $pic1→pixel(55, 55);
  code→printColor($c3);
  $c3 := $pic1→pixel(55, 75);
  code→printColor($c3);
}

action printColor(c: Color) {
  $c→B→to_string→post_to_wall;
  $c→B→to_string→post_to_wall;
  $c→R→to_string→post_to_wall;
  $c→A→to_string→post_to_wall;
  meta private;
}

