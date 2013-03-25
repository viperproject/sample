meta version "v2.2,nothing";
meta name "Latest SEC filings";
meta icon "NewPage";
meta color "#ff008000";
// Retrieves latest SEC filings for selected companies
meta platform "current";

action get_SEC_filings(CIKorTicker: String) returns links1: Link_Collection {
  $s4 := "HTTP://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=" ∥ $CIKorTicker ∥ "&type=&dateb=&owner=exclude&start=0&count=5&output=atom";
  $NS := "{http://www.w3.org/2005/Atom}";
  $s1 := web→download($s4);
  $xml1 := web→xml($s1);
  if data→v→is_invalid or data→v→count > 0 then {
    data→v := collections→create_link_collection;
  }
  $links := data→v;
  foreach xml in $xml1 where $xml→name→equals($NS ∥ "entry") do {
    $s2 := $xml→child($NS ∥ "link")→attr("href");
    $s3 := $xml→child($NS ∥ "title")→value;
    $links→add(web→link_url($s3, $s2));
  }
  $links1 := $links;
  meta private;
}

action set_labels() {
  if data→b→is_invalid then {
    $board := media→create_board(100);
    data→b := $board;
  }
  if data→v→is_invalid then {
    data→v := collections→create_link_collection;
  }
  // To change the list of companies, edit the ticker list in the string below.
  $s2 := "DOW,IBM,MSFT,AAPL,DIS";
  $strings := $s2→split(",");
  data→labels := $strings;
  $m := math→create_number_map;
  foreach s6 in data→labels where true do {
    $m→set_at(data→labels→index_of($s6, 0), code→estimate_label_width($s6));
  }
  data→label_widths := $m;
  code→create_sprites_from_labels;
  time→stop;
}

action main() {
  // This script is designed to allow the user to easily check the latest SEC filings for a set list of companies of interest.
  // It will probably be most interesting to the general TouchDevelop community for its use of text sprites to create a swipe menu.  I realize this may not follow Windows Phone 7 UX guidelines, but cut me some slack -- it is just an experiment.
  // You can customize the list of companies by editing the string s2 in the set labels action.
  data→loop_counter := 0;
  if data→b→is_invalid then {
    code→set_labels;
  }
  data→b→set_friction(.0331);
  data→b→post_to_wall;
  if data→selected→is_invalid→equals(false) then {
    code→select(data→selected);
  }
}

action Knuth_mod(a: Number, n: Number) returns r: Number {
  $r := $a - ($n * math→floor($a / $n));
  meta private;
}

action create_sprites_from_labels() {
  if data→s→is_invalid then {
    $sprite_set1 := data→b→create_sprite_set;
    data→s := $sprite_set1;
  }
  while data→s→count > 0 do {
    data→s→remove_first;
  }
  $board := media→create_board(100);
  $x := 2.25;
  $height := 100;
  $fontSize := 50;
  $sprite_set := $board→create_sprite_set;
  data→total_label_length := 0;
  for 0 ≤ i < data→labels→count do {
    $sprite := $board→create_text(data→label_widths→at($i) * $x, $height, $fontSize, data→labels→at($i));
    data→total_label_length := data→total_label_length + data→label_widths→at($i) * $x;
    $sprite→set_pos($sprite→width / 2, $sprite→height / 2);
    while $sprite→overlap_with($sprite_set)→count > 0 do {
      $sprite→move(1, 0);
      $sprite→set_color(colors→gray);
    }
    $sprite_set→add($sprite);
    $sprite→show;
  }
  data→s := $sprite_set;
  data→b := $board;
  meta private;
}

action estimate_label_width(s1: String) returns total_width: Number {
  $character_lengths_in_string := "11,13,11,13,12,9,13,12,3,4,11,3,20,12,14,13,13,7,10,9,12,13,20,12,13,12,18,13,14,15,10,10,16,15,7,7,14,10,21,17,19,12,19,14,12,12,14,17,26,15,15,16\n\n \n";
  $alphabets_in_string := "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z";
  $alphabets := $alphabets_in_string→split(",");
  $relative_character_lengths := math→create_number_map;
  $strings := $character_lengths_in_string→split(",");
  for 0 ≤ i < $strings→count do {
    $relative_character_lengths→set_at($i, $strings→at($i)→to_number);
  }
  $x := 0;
  foreach s2 in $s1 where true do {
    $x := $x + $relative_character_lengths→at($alphabets→index_of($s2, 0)) + 2;
  }
  $total_width := $x;
  meta private;
}

action select(sprite1: Sprite) {
  wall→clear;
  code→snap_to_center($sprite1);
  foreach sprite in data→s where true do {
    $sprite→set_color(colors→gray);
  }
  data→selected := $sprite1;
  data→selected→set_color(colors→white);
  data→b→update_on_wall;
  $links := code→get_SEC_filings(data→selected→text);
  foreach link in $links where $link→is_invalid→equals(false) do {
    $link→post_to_wall;
  }
  data→b→post_to_wall;
  meta private;
}

action snap_to_center(sprite1: Sprite) {
  $center_x := 240;
  $delta_x := $center_x - $sprite1→x;
  foreach sprite in data→s where true do {
    $sprite→move($delta_x, 0);
    $sprite→set_speed_x(0);
  }
  ... ;
  meta private;
}

action snap_to_left(sprite1: Sprite) {
  $left_position := $sprite1→width / 2;
  $delta_x := $left_position - $sprite1→x;
  foreach sprite in data→s where true do {
    $sprite→move($delta_x, 0);
    $sprite→set_speed_x(0);
  }
  ... ;
  meta private;
}
var labels : String_Collection {
}
var label_widths : Number_Map {
}
var b : Board {
}
var s : Sprite_Set {
}
var selected : Sprite {
}
var total_label_length : Number {
}

event gameloop() {
  foreach sprite in data→s where $sprite→x < - 50 or $sprite→x > data→total_label_length - 50 do {
    $sprite→set_x(code→Knuth_mod($sprite→x + 50, data→total_label_length) - 50);
  }
  for 0 ≤ i1 < 3 do {
    data→b→update_on_wall;
    data→b→evolve;
  }
  meta private;
}

event tap_sprite_in_s(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  foreach sprite1 in data→s where true do {
    $sprite1→set_color(colors→gray);
  }
  $sprite→set_color(colors→white);
  code→select($sprite);
  data→b→update_on_wall;
  meta private;
}

event swipe_sprite_in_s(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  foreach sprite1 in data→s where true do {
    $sprite1→move($delta_x, 0);
    $sprite1→set_speed_x($delta_x * 5);
  }
  data→b→evolve;
  data→b→update_on_wall;
  meta private;
}

event drag_sprite_in_s(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  foreach sprite1 in data→s where true do {
    $sprite1→move($delta_x, 0);
  }
  data→b→evolve;
  data→b→update_on_wall;
  meta private;
}

event swipe_board\u003A_b(x: Number, y: Number, delta_x: Number, delta_y: Number) {
  foreach sprite1 in data→s where true do {
    $sprite1→set_speed_x(($sprite1→speed_x + $delta_x) * 5);
  }
  data→b→evolve;
  data→b→update_on_wall;
  meta private;
}
var loop_counter : Number {
}
var v : Link_Collection {
}

