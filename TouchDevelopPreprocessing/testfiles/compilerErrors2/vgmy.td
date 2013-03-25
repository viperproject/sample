meta version "v2.2,nothing";
meta name "all things games";
meta icon "documents";
// All the things you need to know about games
meta platform "current";
meta import super_lib {
  pub ""
  usage {
  }
}
meta import data_pic {
  pub "epae"
  usage {
  }
  resolve base_string = ♻base_string with {
  }
  resolve json_core = ♻json_core with {
  }
}
meta import base_string {
  pub "dyeu"
  usage {
  }
}
meta import json_core {
  pub "jtfn"
  usage {
  }
}

action main() {
  "Halo 4: information will be posted soon";
}

event tap_wall_Contact(item: Contact) {
  skip;
  meta private;
}

event gameloop() {
  skip;
  meta private;
}

event tap_wall_Place(item: Place) {
  skip;
  meta private;
}
var a : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?c=hVXscIza&w=320&h=480&i=http%3A%2F%2Fwww.justpushstart.com%2Fwp-content%2Fuploads%2F2012%2F04%2FHalo4New-1.jpg";
}
table thing {
  type = "Index";
}

event tap_wall_Picture_Album(item: Picture_Album) {
  skip;
  meta private;
}
var pic : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?c=hVXscIza&w=320&h=480&i=http%3A%2F%2Fwww.justpushstart.com%2Fwp-content%2Fuploads%2F2012%2F04%2FHalo4New-1.jpg";
}

