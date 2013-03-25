meta version "v2.2,nothing";
meta name "button icon search";
meta icon "stacks";
meta color "#ff008000";
// Tap the textbox for more information.
meta platform "current";

action go() {
  wall→clear;
  $statement := "";
  $s := "Search the start(s), the end(e) or the words(w)?";
  if code→askb("Search or not?", 1) then {
    $statement := code→asks("Statement?");
    if code→asks($s)→equals("s") then {
      $statement := $statement→to_upper_case;
      code→icons_show(true, $statement, 0);
    }
    else {
      if code→asks($s)→equals("e") then {
        $statement := $statement→to_upper_case;
        code→icons_show(true, $statement, 2);
      }
      else {
        code→icons_show(true, $statement, 1);
      }
    }
  }
  else {
    code→icons_show(false, "", 0);
  }
  meta private;
}
var buttons : Sprite_Set {
}

action estimate_label_width(s1: String) returns total_width: Number {
  $character_lengths_in_string := "11,13,11,13,12,9,13,12,3,4,11,3,20,12,14,13,13,7,10,9,12,13,20,12,13,12,18,13,14,15,10,10,16,15,7,7,14,10,21,17,19,12,19,14,12,12,14,17,26,15,15,16\n\n \n";
  $alphabets_in_string := "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z";
  $alphabets := $alphabets_in_string→split(",");
  $relative_character_lengths := math→create_number_map;
  $strings := $character_lengths_in_string→split(",");
  $x := 0;
  if not $s1→is_empty then {
    for 0 ≤ i < $strings→count do {
      $relative_character_lengths→set_at($i, $strings→at($i)→to_number);
    }
    $x := 0;
    foreach s2 in $s1 where true do {
      $x := $x + $relative_character_lengths→at($alphabets→index_of($s2, 0)) + 4.5;
    }
  }
  $total_width := $x;
  meta private;
}

action icons_show(search: Boolean, statement: String, scribe: Number) returns done: Boolean {
  // scribe:0 or others for starting with statement;1 for containing it;2 for ending
  $size := 50;
  if $search→equals(false) then {
    foreach s in media→icon_names where true do {
      data→name := wall→create_text_box($s, $size);
      data→name→post_to_wall;
    }
  }
  else {
    if $statement→is_empty then {
      if code→askb("No statements?", 0) then {
        foreach s1 in media→icon_names where true do {
          data→name := wall→create_text_box($s1, $size);
          data→name→post_to_wall;
        }
      }
      else {
        $done := false;
      }
    }
    else {
      if $scribe = 1 then {
        foreach s2 in media→icon_names where $s2→contains($statement) do {
          data→name := wall→create_text_box($s2, $size);
          data→name→post_to_wall;
        }
      }
      else {
        if $scribe = 2 then {
          foreach s3 in media→icon_names where $s3→ends_with($statement) do {
            data→name := wall→create_text_box($s3, $size);
            data→name→post_to_wall;
          }
        }
        else {
          foreach s4 in media→icon_names where $s4→starts_with($statement) do {
            data→name := wall→create_text_box($s4, $size);
            data→name→post_to_wall;
          }
        }
      }
    }
  }
  $done := true;
  meta private;
}
var name : TextBox {
}

action askb(text: String, caption: Number) returns blnResult: Boolean {
  $caption_text := "";
  if $caption = 0 then {
    $caption_text := "warning";
  }
  else {
    $caption_text := "information";
  }
  $blnResult := wall→ask_boolean($text, $caption_text);
  wall→clear;
  meta private;
}

action asks(text: String) returns strResult: String {
  $strResult := wall→ask_string($text);
  wall→clear;
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  wall→push_new_page;
  $board := media→create_board(100);
  $d := collections→create_string_map;
  $sprite_set := $board→create_sprite_set;
  $s := $item→text;
  $board→post_to_wall;
  $d→set_at("height", "100");
  $d→set_at("icon", $s);
  $d→set_at("label", $s);
  $sprite := code→create_button($board, $d);
  $sprite→set_pos(50, 50);
  $board→update_on_wall;
  meta private;
}
var page : Page {
}

action main() {
  $b1 := wall→add_button("search", "search");
}

event tap_wall_Page_Button(item: Page_Button) {
  code→go;
  meta private;
}

action create_button(board1: Board, parameters: String_Map) returns button: Sprite {
  $button := $board1→create_picture(media→create_picture(96, 96));
  $pic := media→create_picture(96, 96);
  ... ;
  $pic→colorize(colors→accent, colors→background, 1);
  $pic→blend(media→large_icon($parameters→at("icon")), 0, 0, 0, 1);
  if $parameters→keys→contains("height") and $parameters→keys→contains("width") then {
    $pic→resize($parameters→at("width")→to_number, $parameters→at("height")→to_number);
  }
  $sprite := $board1→create_picture($pic);
  if $parameters→keys→contains("x") and $parameters→keys→contains("y") then {
    $sprite→set_pos($parameters→at("x")→to_number, $parameters→at("y")→to_number);
  }
  if $parameters→keys→contains("ID") then {
    $loc1 := locations→create_location(90, $parameters→at("ID")→to_number);
    $sprite→set_location($loc1);
  }
  else {
    $x1 := math→random_normalized;
    $loc := locations→create_location(90, $x1);
    $sprite→set_location($loc);
  }
  $button := $sprite;
  meta private;
}

