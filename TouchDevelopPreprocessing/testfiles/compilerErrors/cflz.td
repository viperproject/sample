meta version "v2.2,nothing";
meta name "which card to use";
meta icon "creditcard";
meta color "#ff007fff";
// In case you hold multiple cards, helps you find card allowing you maximum duration for pay by date. Assumes all cards follow standard pay by date of 15  to 20 days from the bill date
meta platform "current,phone";

action main() {
  if data→init2 = 0 then {
    code→init;
    data→init2 := 1;
  }
  foreach s in data→actions where true do {
    code→createText($s);
  }
  $s1 := code→swipecard();
  wall→set_foreground(colors→red);
  if $s1→is_invalid or $s1→is_empty or $s1→equals("") then {
    $s1 := "Couldn\'t determine the card to be swiped";
  }
  else {
    $s1 := "You can swipe:" ∥ $s1;
  }
  ... ;
  $s1→post_to_wall;
}
table cardDates {
  // bill dates
  type = "Index";
  keys {
    s: Number
  }
  fields {
    cardName: String_field
    billdate: Number_field
  }
}

action addCard() {
  $ind := - 1;
  $cName := wall→ask_string("Enter Card Name")→to_upper_case;
  while($cName→is_invalid) or ($cName→is_empty) do {
    $cName := wall→ask_string("Invalid. Re-enter card name")→to_upper_case;
  }
  $x := code→getIndex($cName);
  ... ;
  if $x = - 1 then {
    $ind := records→cardDates_index→count;
    time→log("Adding id:" ∥ $ind);
    time→log("Count:" ∥ records→cardDates_index→count→to_string);
  }
  else {
    $ind := $x;
  }
  $b := code→setCard($cName, $ind);
  $status := "";
  if $b then {
    $status := "Card added";
  }
  else {
    $status := "Card not added";
  }
  wall→pop_page;
  wall→push_new_page;
  wall→set_foreground(colors→red);
  $status→post_to_wall;
  code→main;
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  $x := $item→text;
  wall→push_new_page;
  wall→set_title($x→to_upper_case);
  if "Add Card" →equals($x) then {
    code→addCard;
  }
  if "Delete Card" →equals($x) then {
    code→deletecard;
  }
  if $x→ends_with(":Delete") then {
    code→delete($x);
  }
  meta private;
}

action getIndex(s1: String) returns index: Number {
  $index := - 1;
  foreach c in records→cardDates_index where $c→cardName→get→equals($s1) do {
    $index := $c→s;
  }
  meta private;
}

action setCard(name: String, id: Number) returns b1: Boolean {
  $b1 := false;
  wall→clear;
  $x1 := wall→ask_number("Enter Card Bill date");
  if($x1 ≤ 31) and ($x1 > 0) and (math→ceiling($x1) = $x1) then {
    records→cardDates_index→at($id)→cardName→set($name);
    records→cardDates_index→at($id)→billdate→set($x1);
    $b1 := true;
  }
  else {
    wall→prompt("Invalid Bill date");
  }
  meta private;
}

action createText(s1: String) {
  $tb1 := wall→create_text_box($s1, 32);
  $tb1→set_background(colors→transparent);
  $tb1→post_to_wall;
  meta private;
}
var actions : String_Collection {
}

action init() {
  data→actions := collections→create_string_collection;
  data→actions→add("Delete Card");
  data→actions→add("Add Card");
  meta private;
}
var init2 : Number {
}

action deletecard() {
  $cards := collections→create_string_collection;
  wall→pop_page;
  wall→push_new_page;
  foreach c in records→cardDates_index where true do {
    code→createText($c→cardName→get ∥ ":Delete");
  }
  meta private;
}

action delete(x: String) {
  $card1 := $x→split(":")→at(0);
  $x1 := code→getIndex($card1);
  time→log("To be deleted:" ∥ $card1 ∥ " index:" ∥ $x1);
  while $x1 < records→cardDates_index→count - 1 do {
    records→cardDates_index→at($x1)→cardName→set(records→cardDates_index→at($x1 + 1)→cardName→get);
    records→cardDates_index→at($x1)→billdate→set(records→cardDates_index→at($x1 + 1)→billdate→get);
    $x1 := $x1 + 1;
  }
  records→cardDates_index→at($x1)→clear_fields;
  $delStatus := $card1 ∥ " Deleted";
  wall→pop_page;
  wall→push_new_page;
  wall→set_foreground(colors→red);
  $delStatus→post_to_wall;
  code→main;
  meta private;
}

action swipecard() returns cardname: String {
  $cardname := "";
  $dt := time→today→date→day;
  $max1 := 0;
  $max1ind := - 1;
  foreach c in records→cardDates_index where $c→billdate→get ≤ $dt do {
    if $max1 < $c→billdate→get then {
      $max1 := $c→billdate→get;
      $max1ind := $c→s;
    }
  }
  if $max1ind = - 1 then {
    foreach c1 in records→cardDates_index where $c1→billdate→get ≥ $dt do {
      if $max1 < $c1→billdate→get then {
        $max1 := $c1→billdate→get;
        $max1ind := $c1→s;
      }
    }
  }
  $cardname := records→cardDates_index→at($max1ind)→cardName→get;
  meta private;
}

