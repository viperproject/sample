meta version "v2.2,nothing";
meta name "Тест";
//
meta platform "current";

action main() {
  123→post_to_wall;
}
table thing {
  type = "Index";
  keys {
    s: String
  }
  fields {
    x: Number_field
  }
}

