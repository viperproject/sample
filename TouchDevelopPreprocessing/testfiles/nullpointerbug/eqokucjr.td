meta version "v2.2,nothing";
meta name "PhoneCall";
meta icon "phone";
meta color "#ff007fff";
// This app change a phone number before a call
meta platform "current";

action main() {
  wall→clear;
  wall→clear_buttons;
  wall→set_title("");
  wall→create_text_box("When hitting the buttons bellow, you can change the operator code, call any number or call a number from your address book", 32)→post_to_wall;
  wall→add_button("feature.settings", "Operator");
  wall→add_button("favs", "Call");
  //
  //

}
var operator : Number {
}

action GetOperatorCode() {
  wall→clear;
  $x1 := data→operator;
  if $x1→is_invalid then {
    $x1 := 15;
  }
  $x := wall→ask_number("Operator code: " ∥ $x1→to_string);
  if $x > 0 then {
    data→operator := $x;
  }
  code→main;
  meta private;
}

action GetPhoneNum() {
  $s := phone→choose_phone_number;
  $num := "";
  if data→operator→is_invalid then {
    code→GetOperatorCode;
  }
  if not $s→is_invalid then {
    wall→clear;
    $num := ($s ∥ "")→replace(" ", "")→replace("-", "");
    $s1 := $num;
    $num := $num→replace_regex("^\\+55", "");
    if $num→starts_with("0") and $num→count > 3 then {
      $num := $num→replace_regex("^0..", "");
    }
    $num→copy_to_clipboard;
    $x := wall→ask_number("Number to call: " ∥ $num);
    if not $x→is_invalid and $x > 0 then {
      $num := $x→to_string→trim(" \t");
    }
    if $num→count ≥ 10 and not $num→starts_with("*") and not $num→starts_with("+") and not $num→starts_with("#") then {
      $num := "0" ∥ data→operator→to_string ∥ $num;
    }
    phone→dial_phone_number($num);
    wall→clear;
    if not $num→equals($s1) then {
      if wall→ask_boolean($num, "Phone number") then {
        phone→save_phone_number($num);
      }
    }
  }
  code→main;
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("Operator") then {
    code→GetOperatorCode;
  }
  else {
    if $item→text→equals("Call") then {
      code→GetPhoneNum;
    }
    else {
      $item→text→post_to_wall;
    }
  }
  meta private;
}

