meta version "v2.2,nothing";
meta name "phone number";
// Phone number to text and voice
meta platform "current";

action below_ten(in_number: Number) returns out_text: String {
  $s := "";
  ... ;
  if $in_number = 1 then {
    $s := "One";
  }
  if $in_number = 2 then {
    $s := "Two";
  }
  if $in_number = 3 then {
    $s := "Three";
  }
  if $in_number = 4 then {
    $s := "Four";
  }
  if $in_number = 5 then {
    $s := "Five";
  }
  if $in_number = 6 then {
    $s := "Six";
  }
  if $in_number = 7 then {
    $s := "Seven";
  }
  if $in_number = 8 then {
    $s := "Eight";
  }
  if $in_number = 9 then {
    $s := "Nine";
  }
  if $in_number = 0 then {
    $s := "Zero";
  }
  $out_text := $s;
  meta private;
}

action text_action(in_string: String) returns out_text: String {
  ... ;
  $s_ph := $in_string;
  ... ;
  $ph := "";
  ... ;
  foreach s in $s_ph where true do {
    if not $s→to_number→is_invalid then {
      $s1 := code→below_ten($s→to_number);
      $ph := $ph ∥ " " ∥ $s1;
    }
    if $s→equals("+") then {
      $ph := $ph ∥ " plus ";
    }
    ... ;
  }
  ... ;
  $out_text := $ph;
  meta private;
}
var v : Number {
}

action start() {
  $link := phone→choose_phone_number;
  $s := $link→address;
  $s1 := code→text_action($s);
  $s1→post_to_wall;
  ... ;
  $sound := languages→speak("en", $s1);
  $sound→play;
  ... ;
}

