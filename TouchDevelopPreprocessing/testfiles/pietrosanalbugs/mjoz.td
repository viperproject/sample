meta version "v2.2,nothing";
meta name "Japanese animal matching game";
meta icon "ThreeColumn";
meta color "#ff00008B";
//
meta platform "current";

action main() {
  code→menu;
}

action menu() {
  // Collection of the text for the TextBoxes, add in reverse order.
  wall→clear;
  data→board := media→create_landscape_board(800, 480);
  $x := collections→create_string_collection;
  $x→add("cat");
  $x→add("bird");
  $x→add("dog");
  $x→add("fish");
  data→s4 := $x→random;
  $s2 := languages→translate("en", "ja", data→s4);
  $x2 := data→board→create_text(100, 20, 40, $s2);
  $x2→set_pos(25, 10);
  $x2→set_color(colors→black);
  data→board→post_to_wall;
  $strings := collections→create_string_collection;
  for 0 ≤ i < 4 do {
    $s3 := $x→random;
    $strings→add($s3);
    $x→remove($s3);
  }
  wall→set_title("Choose correct word.");
  foreach s in $strings where true do {
    // Build the TextBoxes in a little loop and post to the wall.
    $tb := wall→create_text_box($s, 40);
    $tb→set_background(colors→blue);
    $tb→set_foreground(colors→yellow);
    $tb→post_to_wall;
  }
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  // Turn the item into text and push into a variable.
  $tbItem := $item→text;
  // Check the variable against the textbox text for a match
  if $tbItem→equals(data→s4) then {
    // Then carry out some action
    wall→clear;
    wall→prompt("Correct!");
    code→menu;
  }
  else {
    wall→clear;
    wall→prompt("Incorrect!");
    code→menu;
  }
  meta private;
}
var board : Board {
}
var s4 : String {
}

