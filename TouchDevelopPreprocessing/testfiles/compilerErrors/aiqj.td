meta version "v2.2,nothing";
meta name "my script";
//
meta platform "current";
meta import a_library {
  pub ""
  usage {
  }
}

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

event tap_wall_Device(item: Device) {
  // Waveing the picture
  meta private;
}
var v : Camera {
  // open camera
}
table thing {
  type = "Table";
}
var a : Picture {
  is\_resource = true;
}

