meta version "v2.2,nothing";
meta name "my script";
//
meta platform "current";

action main() {
  time→now→second→post_to_wall;
  time→now→minute→post_to_wall;
  time→now→hour→post_to_wall;
}

