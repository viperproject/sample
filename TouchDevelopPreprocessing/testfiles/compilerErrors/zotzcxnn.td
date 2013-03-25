meta version "v2.2,nothing";
meta name "Thank You";
meta icon "SmilieHappy";
meta color "#ff00008B";
//
meta platform "current";

action main() {
  wall→set_reversed(true);
  art→Say_Thanks→post_to_wall;
  code→To2;
  code→Message;
  skip;
  skip;
  skip;
  skip;
  skip;
  skip;
  skip;
  if data→Choice = 0 then {
    data→Tweet := data→Message_1;
  }
  if data→Choice = 1 then {
    data→Tweet := data→Message_2;
  }
  if data→Choice = 2 then {
    data→Tweet := data→Message_3;
  }
  skip;
  wall→ask_boolean("", "Tweet");
  skip;
  skip;
  skip;
  skip;
}
var Capital_Purple : Color {
  // Capital Purple
  is\_resource = true;
  url = "#ff380982";
}
var Say_Thanks : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/cvkzfnpj";
}
var To : String {
}
var \u0040 : String {
  is\_resource = true;
  url = "data:text/plain;base64,QA==";
}
var Messages : String_Collection {
}
var strings : String_Collection {
}
var Choice : Number {
}
var Message_Select : String {
}
var Tweet : String {
}
var Message_1 : String {
}
var Message_2 : String {
}
var Message_3 : String {
}
var Studio_E : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/iqxcychj";
}
var Studio_A : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/erkksmhj";
}
var Mees : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/fengbwup";
}
var strings2 : String_Collection {
}
var Choose_Photo : Number {
}
var Photo : Picture {
}

action To2() {
  data→strings := collections→create_string_collection;
  data→strings→add("Thank you for sharing your knowledge!");
  data→strings→add("Thank you for visiting Capital University!");
  data→strings→add("Thank you for giving!");
  skip;
  data→To := ("@" ∥ wall→ask_string("username"));
  data→To→post_to_wall;
  meta private;
}

action Message() {
  data→Choice := wall→pick_string("", "", data→strings);
  data→Message_1 := " Thank you for sharing your knowledge! #capfam";
  data→Message_2 := " Thank you for visiting Capital University! #capfam";
  data→Message_3 := " Thank you for giving! #capfam";
  data→Tweet→post_to_wall;
  meta private;
}
var pic : Picture {
  is\_resource = true;
}
table thing {
  type = "Object";
}
var Capital_Purple2 : Color {
  // Capital Purple
  is\_resource = true;
  url = "#ff380982";
}

action Main() {
  if box→is_init then {
    skip;
    skip;
    skip;
  }
  if true then {
    skip;
    skip;
    skip;
  }
}

