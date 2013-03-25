meta version "v2.2,nothing";
meta name "my script";
//
meta platform "current";
meta import todo_item_collection {
  pub "vjuv"
  usage {
  }
  resolve todo_item = ♻todo_item with {
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection with {
  }
  resolve color_helpers = ♻color_helpers resolve instance = ♻instance resolve definition = ♻definition}
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
meta import collection {
  pub "qfhi"
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
meta import instance {
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
meta import definition {
  pub "sngp"
  usage {
  }
  resolve core = ♻core resolve array = ♻array with {
  }
  resolve collection = ♻collection with {
  }
}
meta import todo_item {
  pub "cpfp"
  usage {
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection resolve instance = ♻instance with {
  }
  resolve color_helpers = ♻color_helpers resolve definition = ♻definition with {
  }
}

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}
var a : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?i=http%3a%2f%2fart326mollyandkara.files.wordpress.com%2f2011%2f09%2fhow-to-draw-a-spaceship.jpg&w=252&h=252&t=http%3a%2f%2fts1.mm.bing.net%2fimages%2fthumbnail.aspx%3fq%3d4773032120418496%26id%3d1463ca4500f8c22cd1c7c2634a15ecf4&c=LhxcwJjj";
}
table thing {
  type = "Table";
  fields {
    @2: Number_field
  }
}

