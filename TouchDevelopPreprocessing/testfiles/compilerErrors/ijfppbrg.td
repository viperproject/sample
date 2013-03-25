meta version "v2.2,nothing";
meta name "new";
meta icon "123";
meta color "#ff800080";
//
meta platform "current";

action main() {
  "Hello world!" →post_to_wall;
  player→active_song;
}

action show() {
  if box→is_init then {
    skip;
  }
  if true then {
    skip;
  }
}

event phone_face_up() {
  wall→set_background(colors→random);
  meta private;
}
var pic : Picture {
  is\_resource = true;
}
var v : String {
}
table thing {
  type = "Object";
}
table thing2 {
  type = "Table";
  fields {
    f: Number_field
    f2: Number_field
    f3: Number_field
    f4: Number_field
  }
}

