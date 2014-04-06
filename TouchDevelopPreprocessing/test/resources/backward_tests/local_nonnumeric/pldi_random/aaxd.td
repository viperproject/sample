//:: ExpectedOutput(backward.counterexample.found)
action main() {
  $loc := locations→create_location(47.5, - 121.9);
  $loc1 := locations→create_location($loc→latitude + math→rand(10) / 100, $loc→longitude + math→rand(10) / 100);
  $ls := maps→directions($loc, $loc1, true);
  $map := maps→create_map;
  $map→post_to_wall;
  //:: ExpectedOutput(assert.failed)
  $map→add_line($ls, colors→accent, 6);
}
