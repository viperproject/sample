// Textmaster, Circler, Waller and Starouz: Copyright Pouya Animation Inc
//:: ExpectedOutput(backward.counterexample.found)
action Waller() {
  // this code will get a text from user
  wall->create_text_box("note: Waller will make a wallpaper from a text, that this text will write in a some random places, so now you should enter that text.", 18)->post_to_wall;
  $text := wall->ask_string("Enter a text to make your wallpaper");
  wall->clear;
  // this code will get a font size from user
  wall->create_text_box("note: Waller will make a wallpaper with some texts that the texts have a muximum font size and now you should write this maximum size", 18)->post_to_wall;
  $font := wall->ask_number("Please write a maximum size for font");
  // This code will ask user a number for next loop (that how many user need text)
  wall->clear;
  wall->create_text_box("note: Waller will put some texts in some places now hamany text you want?", 18)->post_to_wall;
  $i := wall->ask_number("how many text you want?");
  // This code will ask users screen size
  wall->clear;
  $w := wall->ask_number("enter your phones screens width");
  // Will ask users screens height
  wall->clear;
  $H := wall->ask_number("Enter your screens height");
  // This is code will create a picture for Walling
  wall->clear;
  wall;
  //:: ExpectedOutput(assert.failed)
  $pic := media->create_picture($w, $H);
  $pic->post_to_wall;
  // This code will drow texts in their places.
  for 0 ≤ i1 < $i do {
    //:: ExpectedOutput(assert.failed)
    $pic->draw_text(math->rand($w), math->rand($w), $text, $font, math->rand(360), colors->rand);
    $pic->update_on_wall;
  }
  // This Code will ask user about saving picture
  wall->clear;
  if wall->ask_boolean("waller@termini :~$", "do you want to save it? it will save in your photo gallery") then {
    $pic->save_to_library;
  }
}