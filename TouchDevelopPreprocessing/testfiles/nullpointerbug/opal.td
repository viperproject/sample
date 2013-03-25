meta version "v2.2,nothing";
meta name "my scb";
meta icon "addressbook";
meta color "#ff007fff";
//
meta platform "current";

action main() {
  data→z := wall→ask_boolean("Login?", "");
  if data→z→equals(true) then {
    wall→clear;
    data→x := wall→ask_number("Enter your User name");
    wall→clear;
    if data→x = 232984722 then {
      data→y := wall→ask_number("Enter your password ");
      code→go232984722;
    }
    else {
      if data→x = 155555 then {
        data→y := wall→ask_number("Enter your password ");
        code→go155555;
      }
      else {
        if data→x = 234811 then {
          data→y := wall→ask_number("Enter your password ");
          code→go234811;
        }
        else {
          if data→x = 654321 then {
            data→y := wall→ask_number("Enter your password ");
            code→go654321;
          }
          else {
            if data→x = 0 then {
              data→y := wall→ask_number("Enter your password ");
              code→go5;
            }
          }
        }
      }
    }
  }
  else {
    wall→clear;
    data→x := wall→ask_number("Enter your User name");
    wall→clear;
    data→y := wall→ask_number("Enter your password ");
    wall→clear;
    time→sleep(60);
    time→stop_and_close;
  }
}
table T232984722\'s_sct {
  type = "Table";
  fields {
    eng: String_field
    chn: String_field
  }
}
var x : Number {
}

action go232984722() {
  if data→x = 232984722 and data→y = 234811 then {
    wall→clear;
    records→T232984722\'s_sct_table→clear;
    $r := records→T232984722\'s_sct_table→add_row;
    $r→eng→set("excellent");
    $r→chn→set("优秀的");
    $r1 := records→T232984722\'s_sct_table→add_row;
    $r1→eng→set("good");
    $r1→chn→set("好");
    records→T232984722\'s_sct_table→post_to_wall;
    while true do {
      wall→ask_string("+what?");
      time→stop_and_close;
    }
    while true do {
      time→sleep(1);
    }
  }
  else {
    wall→clear;
    "Wrong password " →post_to_wall;
  }
  meta private;
}
var y : Number {
}
table T155555\'s_sct {
  type = "Table";
  fields {
    eng: String_field
    chn: String_field
  }
}
table T654321\'s_sct {
  type = "Table";
  fields {
    eng: String_field
    chn: String_field
  }
}
table thing3 {
  type = "Table";
  fields {
    eng: String_field
    chn: String_field
  }
}
table T234811\'s_sct {
  type = "Table";
  fields {
    eng: String_field
    chn: String_field
  }
}

action go155555() {
  if data→x = 155555 and data→y = 310031 then {
    wall→clear;
    records→T155555\'s_sct_table→clear;
    $r := records→T155555\'s_sct_table→add_row;
    $r→eng→set("excellent");
    $r→chn→set("优秀的");
    $r1 := records→T155555\'s_sct_table→add_row;
    $r1→eng→set("good");
    $r1→chn→set("好");
    $r2 := records→T155555\'s_sct_table→add_row;
    $r2→eng→set("bad");
    $r2→chn→set("坏");
    records→T155555\'s_sct_table→post_to_wall;
    while true do {
      wall→ask_string("+what?");
      time→stop_and_close;
    }
    while true do {
      time→sleep(1);
    }
  }
  else {
    wall→clear;
    "Wrong password " →post_to_wall;
  }
  meta private;
}

action go234811() {
  if data→x = 234811 and data→y = 234811 then {
    wall→clear;
    records→T234811\'s_sct_table→clear;
    $r := records→T234811\'s_sct_table→add_row;
    $r→eng→set("excellent");
    $r→chn→set("优秀的");
    $r1 := records→T234811\'s_sct_table→add_row;
    $r1→eng→set("good");
    $r1→chn→set("好");
    $r2 := records→T234811\'s_sct_table→add_row;
    $r2→eng→set("Ning Ning");
    $r2→chn→set("宁宁");
    records→T234811\'s_sct_table→post_to_wall;
    while true do {
      wall→ask_string("+what?");
      time→stop_and_close;
    }
    while true do {
      time→sleep(1);
    }
  }
  else {
    wall→clear;
    "Wrong password " →post_to_wall;
  }
  meta private;
}

action go654321() {
  if data→x = 654321 and data→y = 123456 then {
    wall→clear;
    records→T654321\'s_sct_table→clear;
    $r := records→T654321\'s_sct_table→add_row;
    $r→eng→set("excellent");
    $r→chn→set("优秀的");
    $r1 := records→T654321\'s_sct_table→add_row;
    $r1→eng→set("good");
    $r1→chn→set("好");
    records→T654321\'s_sct_table→post_to_wall;
    while true do {
      wall→ask_string("+what?");
      time→stop_and_close;
    }
    while true do {
      time→sleep(1);
    }
  }
  else {
    wall→clear;
    "Wrong password " →post_to_wall;
  }
  meta private;
}

action go5() {
  if data→x = 0 and data→y = 0 then {
    wall→clear;
    records→thing3_table→clear;
    $r := records→T232984722\'s_sct_table→add_row;
    $r→eng→set("excellent");
    $r→chn→set("优秀的");
    $r1 := records→thing3_table→add_row;
    $r1→eng→set("good");
    $r1→chn→set("好");
    records→thing3_table→post_to_wall;
    while true do {
      wall→ask_string("+what?");
      time→stop_and_close;
    }
    while true do {
      time→sleep(1);
    }
  }
  else {
    wall→clear;
    "Wrong password " →post_to_wall;
  }
  meta private;
}
var z : Boolean {
}

