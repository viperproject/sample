action main() {
  $board := media→create_full_board;
  $clouds := $board→create_sprite_set;
  $numClouds := 8;
  for 0 ≤ i < $numClouds do {
    $pic := media→create_picture(80, 35);
    $cloud := $board→create_picture($pic);
    $clouds→add($cloud);
  }
  $clouds→at(7)→set_x($board→width / 2);
  $clouds→at($clouds→count - 1)→set_x($board→width / 2);
}