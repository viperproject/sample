meta version "v2.2,nothing";
meta name "my script 2-25-2013";
//
meta platform "current";

action main() {
  "Hello world!" →post_to_wall;
  $mycolor := colors→red;
}
table thing {
  type = "Object";
}
table thing2 {
  type = "Object";
  fields {
    f: String_field
  }
}

