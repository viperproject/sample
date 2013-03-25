meta version "v2.2,nothing";
meta name "NineOclock";
//
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

action go() {
  skip;
  meta private;
}

action go2() {
  skip;
  meta private;
}
var v : Number {
}
table thing {
  type = "Object";
  fields {
    x: Board_field
    x0: Number_field
  }
}

