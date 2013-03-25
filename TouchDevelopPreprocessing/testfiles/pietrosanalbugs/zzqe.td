meta version "v2.2,nothing";
meta name "my script";
meta color "#fff4bbff";
// 自由尝试
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
  foreach sprite in media→create_board(640) where true do {
    skip;
  }
}
var v : Number {
}
var pic : Picture {
  is\_resource = true;
}

action go() {
  media→choose_picture→location;
}

