meta version "v2.2,nothing";
meta name "Easy Turtle";
meta icon "tools";
meta color "#ff007fff";
// An easy way to make awesome things with a turtle script.
meta platform "current";
meta import turtle {
  pub "aprqa"
  usage {
    action initialize_portrait()
    action forward(distance: Number)
    action pen_color(c: Color)
    action right_turn(angle: Number)
  }
}

action main() {
  wall→create_text_box("Don\'t forget: making your own turtle script is even more fun!", 19)→post_to_wall;
  data→C := wall→ask_boolean("Change the color of the pen of the turtle with every movement?", "");
  data→F := wall→ask_number("How fast do you want the forward movement of the turtle to grow? If it is 0, the turtle won\'t move forward. I like 0.1");
  data→R := wall→ask_number("How fast do you want the angle that the turtle makes to grow? If it is 0, the turtle will go straight. I like 0.1");
  ♻ turtle→initialize_portrait;
  for 0 ≤ i < 999999 do {
    if data→C→equals(true) then {
      ♻ turtle→pen_color(colors→random);
    }
    ♻ turtle→forward(data→F * $i);
    ♻ turtle→right_turn(data→R * $i);
  }
}
var R : Number {
}
var F : Number {
}
var x : Number {
}
var C : Boolean {
}

