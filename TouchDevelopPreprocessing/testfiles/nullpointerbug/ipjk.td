meta version "v2.2,nothing";
meta name "New Contact";
meta icon "adduser";
meta color "#ffff2800";
// Create a shortcut to this script to create and save contacts much faster than the regular phone UI.
meta platform "current";

action New(First_name\u002A: String, Last_name: String, Phone\u002A: Number) {
  if $First_name\u002A→count = 0 or $Phone\u002A = 0 then {
    skip;
  }
  else {
    $contact := social→create_contact("");
    $contact→set_mobile_phone($Phone\u002A→to_string);
    $contact→set_first_name($First_name\u002A);
    $contact→set_last_name($Last_name);
    social→save_contact($contact);
    $strings := collections→create_string_collection;
    $strings→add("Call");
    $strings→add("Compose Text");
    $strings→add("Save Again");
    while true do {
      wall→clear;
      $x := wall→pick_string("You must tap Save on the previous screen to save the contact to your phone.", "Tap Save Again to retry, or choose another option.", $strings);
      if $x = 0 then {
        phone→dial_phone_number($contact→mobile_phone→address);
      }
      else {
        if $x = 1 then {
          social→send_sms($contact→mobile_phone→address, "Hey, this is ");
        }
        else {
          social→save_contact($contact);
        }
      }
    }
  }
  data→Main_tile→set_background(colors→accent);
}
var Main_tile : Tile {
  readonly = true;
}

