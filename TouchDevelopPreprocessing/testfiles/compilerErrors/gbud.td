meta version "v2.2,nothing";
meta name "Arthurio";
meta icon "approvebutton";
meta color "#ffffdf00";
// Arthurio's official WP8 application
meta platform "current,accelerometer,calendar,gyroscope,home,location,maps,media,microphone,contacts,phone";
meta import a_library {
  pub ""
  usage {
  }
}

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}
table thing {
  type = "Index";
}
var a : Picture {
  is\_resource = true;
  url = "https://sn2files.storage.live.com/y1pWDtSpdpOZTt4HEGZTYTJjCTmmfLt3HC7ljL_nE_SxOr2J2-UDnLguw/WP_20130302%201.jpg?psid=1&ck=0&ex=720";
}

event tap_wall_Picture(item: Picture) {
  media→choose_picture→fill_rect(0, 0, 100, 100, 0, colors→accent);
  meta private;
}

event tap_wall_Song(item: Song) {
  ... ;
  meta private;
}
table thing2 {
  type = "Object";
}

action go() {
  media→songs→play;
  meta private;
}
var a2 : Picture {
  is\_resource = true;
  url = "https://sn2files.storage.live.com/y1pd2HeiIwHCzv9igsxSogkLsBA-p5xV8aTvDY6CdcfKiVzSPge7WcfVI6rrdz5PtlSTY9ahjrZq-AYElhB039yDA/WP_20130302.jpg?psid=1";
}
var a3 : Picture {
  is\_resource = true;
  url = "https://sn2files.storage.live.com/y1pOEvm0aO04DYdMAZTlFqz3eMW9BmkIkfWM_EJQCDPh-bi3Dlix_0D3g/WP_20130302%202.jpg?psid=1&ck=0&ex=720";
}
var Arthurio : Tile {
  // Official
  readonly = true;
}

action go2() {
  data→Arthurio;
  meta private;
}
var a4 : Picture {
  is\_resource = true;
}

event tap_wall_Tile(item: Tile) {
  art→a→flip_horizontal;
  art→a2→flip_horizontal;
  art→a3→flip_horizontal;
  art→a4→flip_horizontal;
  meta private;
}

event tap_wall_Picture_Album(item: Picture_Album) {
  skip;
  meta private;
}

