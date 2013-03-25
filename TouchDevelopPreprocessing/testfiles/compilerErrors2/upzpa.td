meta version "v2.2,nothing";
meta name "button library";
meta icon "fourcolumn";
meta color "#ff004b49";
// Library
meta isLibrary "yes";
meta platform "current";

action main() {
  $d := collections→create_string_map;
  $board := media→create_full_board;
  $sprite_set := $board→create_sprite_set;
  $board→post_to_wall;
  $d→set_at("height", "100");
  $x := 0;
  $y := 0;
  foreach s in media→icon_names where true do {
    $d→set_at("icon", $s);
    $d→set_at("label", $s);
    $sprite := code→create_button($board, $d);
    $sprite→set_pos(50 + 100 * $x, 50 + 100 * $y);
    if $x ≥ 4 then {
      $x := 0;
      if $y < 7 then {
        $y := $y + 1;
      }
      else {
        $board→update_on_wall;
        $y := 0;
      }
    }
    else {
      $x := $x + 1;
    }
    $board→update_on_wall;
  }
  ... ;
}

action create_button(board1: Board, parameters: String_Map) returns button: Sprite {
  $button := $board1→create_picture(media→create_picture(96, 96));
  $pic := media→create_picture(96, 96);
  ... ;
  $pic→colorize(colors→accent, colors→background, 1);
  $pic→blend(media→large_icon($parameters→at("icon")), 0, 0, 0, 1);
  if $parameters→keys→contains("label") then {
    $x := code→estimate_label_width($parameters→at("label"));
    $pic→draw_text($pic→width / 2 - $x / 4, 75, $parameters→at("label"), 16, 0, colors→foreground);
  }
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
var buttons : Sprite_Set {
}
var button_parameters : String_Map {
}

action grid_layout(board1: Board, sprite_set1: Sprite_Set, options: String_Map) {
  $d := collections→create_string_map;
  $board := media→create_full_board;
  $sprite_set := $board→create_sprite_set;
  $board→post_to_wall;
  $d→set_at("height", "100");
  $x := 0;
  $y := 0;
  foreach s in media→icon_names where true do {
    $d→set_at("icon", $s);
    $d→set_at("label", $s);
    $sprite := code→create_button($board, $d);
    $sprite→set_pos(50 + 100 * $x, 50 + 100 * $y);
    if $x ≥ 4 then {
      $x := 0;
      if $y < 7 then {
        $y := $y + 1;
      }
      else {
        $board→update_on_wall;
        $y := 0;
      }
    }
    else {
      $x := $x + 1;
    }
    $board→update_on_wall;
  }
  ... ;
}

action go() {
  skip;
  meta private;
}

