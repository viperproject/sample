meta version "v2.2,nothing";
meta name "GSF13";
//
meta platform "current";
meta import Button_library_1\u002E4 {
  pub "pfvb"
  usage {
  }
}

action firstPage() {
  data→start2 := wall→add_button("next", "Next");
  data→start2→post_to_wall;
  wall→create_text_box("Tap start below to begin the setup process and start using iAccess!", 19)→post_to_wall;
  wall→create_text_box("Hello, and welcome to the first cloud based application designed to allow communication between the deaf and visually impaired.", 19)→post_to_wall;
  wall→create_text_box("iAccess", 50)→post_to_wall;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("Next") then {
    code→secondPage;
  }
  if $item→text→equals("Info") then {
    wall→prompt("Some Info Stuff");
  }
  if $item→text→equals("Share") then {
    code→share;
  }
  if $item→text→equals("Setup") then {
    data→choice := wall→ask_string("Choose Input: Text, Speech or Morse Code.")→to_lower_case;
  }
  if data→choice→equals("text") then {
    code→choseText;
  }
  if data→choice→equals("speech") then {
    code→choseSpeech;
  }
  if data→choice→equals("morse code") then {
    code→choseMorseCode;
  }
  meta private;
}
var start2 : Page_Button {
  // I have no idea why I added this, but will not remove it just in case the code goes strange :)
}

action second() {
  if box→is_init then {
    wall→add_button("share", "Share");
    wall→add_button("questionmark", "Info");
    wall→add_button("sync", "Setup");
    code→drawClock;
  }
  if true then {
    skip;
  }
}

action first() {
  if box→is_init then {
    code→firstPage;
  }
  if true then {
    skip;
  }
}

action main() {
  code→first;
  meta private;
}

action secondPage() {
  code→second;
  meta private;
}

action share() {
  $b := wall→ask_boolean("Would You Like To Share?", "Confirm");
  if $b→equals(true) then {
    web→browse("mailto:?subject=iAccess&body=Hi, Check This Out: URL");
  }
  meta private;
}
var choice : String {
  // this string contains the choice of the users input
}

action choseText() {
  wall→prompt("Chose Text");
  meta private;
}

action choseSpeech() {
  wall→prompt("Chose Speech");
  meta private;
}

action choseMorseCode() {
  wall→prompt("Chose Morse Code");
  meta private;
}

action drawClock() {
  $s1 := time→now→hour→to_string→concat(":")→concat(time→now→minute→to_string);
  $tb := wall→create_text_box($s1, 70);
  $s := "Il est actuellement";
  $s := $s→concat(time→now→hour→to_string→concat(" heure et "));
  $s := $s→concat(time→now→minute→to_string→concat("Minutes"));
  $tb→post_to_wall;
  meta private;
}

