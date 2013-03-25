meta version "v2.2,nothing";
meta name "copy-paste test (⊙o⊙)？";
meta icon "DocumentsAlt";
meta color "#ffFF2800";
//
meta platform "location,maps,search,translation";
meta import Button_library_1\u002E4 {
  pub "aeii"
  usage {
    action create_button(width: Number, text: String, color: Color, scale: Number, horizontal_offset: Number) returns button_picture: Picture
    action create_touched_button(width: Number, text: String, color: Color, scale: Number, horizontal_offset: Number) returns button_picture: Picture
  }
}
meta import Button_library_1\u002E4_my {
  pub "pfvb"
  usage {
    action create_button(width: Number, text: String, color: Color, scale: Number, horizontal_offset: Number) returns button_picture: Picture
    action create_touched_button(width: Number, text: String, color: Color, scale: Number, horizontal_offset: Number) returns button_picture: Picture
  }
}

action copy\u002Dpaste\u003F() {
  // (⊙o⊙)
  ♻ Button_library_1\u002E4→create_button(100, "his", colors→green, 1, 0)→post_to_wall;
  ♻ Button_library_1\u002E4→create_touched_button(100, "his", colors→green, 1, 0)→post_to_wall;
  ♻ Button_library_1\u002E4_my→create_button(100, "my", colors→green, 1, 0)→post_to_wall;
  ♻ Button_library_1\u002E4_my→create_touched_button(100, "my", colors→green, 1, 0)→post_to_wall;
  time→sleep(1);
  $copypaste_boolean := wall→ask_boolean("is it copy-paste?", "1 - yes, 0 - no");
  if $copypaste_boolean then {
    bazaar→post_leaderboard_score(1);
  }
  else {
    bazaar→post_leaderboard_score(0);
  }
  wall→clear;
  bazaar→post_leaderboard_to_wall;
}

