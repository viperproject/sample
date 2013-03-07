meta version "v2.2,nothing";
meta name "ascii art";
// Converts picture into ASCII text.
meta isLibrary "yes";
meta platform "current";

action main() {
  // A sample usage of the library.
  $pic := media→pictures→random;
  $pic→resize( - 1, 120);
  $pic→post_to_wall;
  "Converting to ascii..." →post_to_wall;
  $s := code→convert($pic);
  wall→create_text_box($s, 3)→post_to_wall;
}

action convert(pic: Picture) returns s: String {
  // Converts picture to ascii using 1 character per pixel grey scale technique.
  $pic := $pic→clone;
  $pic→desaturate;
  $w := $pic→width;
  $h := $pic→height;
  $char_table := "░▒▓█";
  $strings := collections→create_string_collection;
  for 0 ≤ i < $w do {
    $l := "";
    for 0 ≤ j < $h do {
      $c := $pic→pixel($i, $j);
      $g := $c→R * ($char_table→count - 1);
      $l := $l ∥ $char_table→at($g);
    }
    $strings→add($l);
  }
  $s := $strings→join("\n");
}

