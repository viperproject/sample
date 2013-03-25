meta version "v2.2,nothing";
meta name "my script";
//
meta platform "current";

action main() {
  $myColor := colors→red;
  wall→set_background($myColor);
  "Hello world!" →post_to_wall;
  for 0 ≤ i < 10 do {
    code→go;
  }
  skip;
}
table thing {
  type = "Object";
}
var v : Number {
}
table thing2 {
  type = "Object";
}

action go() {
  $url := "http://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Roosevelt_University.png/167px-Roosevelt_University.png";
  web→download_picture($url)→post_to_wall;
  $text := "Are you sure?";
  wall→prompt($text);
  meta private;
}

