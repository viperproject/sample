meta version "v2.2,nothing";
meta name "All player";
meta icon "addfolder";
//
meta platform "current";
meta import DateTime {
  pub "rxlqb"
  usage {
  }
}
meta import go_to_picture {
  pub "kkxp"
  usage {
  }
}

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}
var v : Picture_Album {
  // photo
}
var v2 : Camera {
}
var v3 : Media_Player {
}
var v4 : Media_Player {
}
table thing {
  type = "Decorator";
  keys {
    target: Web_Request
  }
}
var a : Picture {
  is\_resource = true;
}
var a2 : Sound {
  is\_resource = true;
}
var a3 : Color {
  is\_resource = true;
  url = "#ff3ebc9a";
}

action go() {
  skip;
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  skip;
  meta private;
}

