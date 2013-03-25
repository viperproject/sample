meta version "v2.2,nothing";
meta name "Тест";
//
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
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

