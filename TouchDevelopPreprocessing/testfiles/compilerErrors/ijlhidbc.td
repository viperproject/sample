meta version "v2.2,nothing";
meta name "Тест";
//
meta platform "current";

action main() {
  records→thing_index→post_to_wall;
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

action go() {
  records→thing_index→clear;
  records→thing_index→at("А")→x→set(1);
  records→thing_index→at("Б")→x→set(2);
  records→thing_index→at("В")→x→set(3);
}

