// Test case with out-of-bound text drawing (user can choose offset)

action main() {
  // Draws text on a picture and saves it to the library
  code→init_email;
  $pic := media→choose_picture;
  if $pic→is_invalid then {
    "No picture chosen." →post_to_wall;
  }
  else {
    code→paint_pic($pic);
  }
}

var email : String {
}

action init_info() {
  data→email := wall→ask_string("Enter your email address...");
  if wall→ask_boolean("Do you want to test your email?", "Just making sure...") then {
    social→send_email(data→email, "Found your phone ... Test email", "Just testing if the email you entered is correct.");
  }
  meta private;
}

//:: ExpectedOutput(backward.counterexample.found)
action paint_pic(pic: Picture) {
  // Resize to screen
  if $pic→is_panorama then {
    $pic→resize( - 1, 800);
  }
  else {
    $pic→resize(480, - 1);
    contract->assert($pic->width = 480, "");
  }
  $pic→fill_rect(0, 0, $pic→width, $pic->height, 0, colors→accent);
  $pic→post_to_wall;
  // Draw text rectangle
  $offset := wall→ask_number("Enter the pixel offset for the text?");

  //:: ExpectedOutput(assert.failed)
  $pic→draw_text(32 + $offset, 32, "Lost phone! Please contact\n" ∥ data→email, 24, 0, colors→white);

  $pic→update_on_wall;
  if wall→ask_boolean("Save?", "You will find the picture in \'saves pictures\'") then {
    $pic→save_to_library;
  }
  meta private;
}

action init_email() {
  // problem: short-circuiting not implemented:  while data→email→is_invalid or data→email→is_empty do {
  while data→email→is_invalid do {
    code→init_info;
  }
  meta private;
}


var v : Number {
}

table thing {
  type = "Object";
  //persistent = false;
}
