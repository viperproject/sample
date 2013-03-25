meta version "v2.2,nothing";
meta name "Date and Time";
//
meta platform "current";
meta import data_instance {
  pub "patj"
  usage {
  }
  resolve core = ♻core with {
  }
  resolve array = ♻array resolve color_helpers = ♻color_helpers with {
  }
  resolve collection = ♻collection with {
  }
}
meta import core {
  pub "jtfn"
  usage {
  }
}
meta import array {
  pub "txuv"
  usage {
  }
  resolve core = ♻core with {
  }
}
meta import color_helpers {
  pub "wszv"
  usage {
  }
}
meta import collection {
  pub "qfhi"
  usage {
  }
  resolve core = ♻core with {
  }
}

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

event tap_wall_Camera(item: Camera) {
  skip;
  meta private;
}

