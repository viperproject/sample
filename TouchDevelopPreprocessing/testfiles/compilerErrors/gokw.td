meta version "v2.2,nothing";
meta name "test3";
// Click on messaging
meta platform "current";
meta import a_library {
  pub ""
  usage {
  }
}

action main() {
  social→create_message("" →at(3659824525));
}

event tap_wall_Message(item: Message) {
  skip;
  meta private;
}

action go() {
  web→play_media("I like baccon");
  meta private;
}
var v : Number {
}
table thing {
  type = "Object";
}
var a : Picture {
  is\_resource = true;
}

action go2() {
  skip;
  meta private;
}

