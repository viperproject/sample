meta version "v2.2,nothing";
meta name "my script";
//
meta platform "current";

action main() {
  "Hello world!" →post_to_wall;
  skip;
}

action show() {
  if box→is_init then {
    skip;
  }
  if true then {
    skip;
  }
}
var pic : Picture {
  is\_resource = true;
}
table thing {
  type = "Object";
}

