meta version "v2.2,nothing";
meta name "GPS coordinates";
meta icon "bullseye";
meta color "#ff007fff";
// This app allows you to get the values of latitude and longitude of your current location or a chosen location. Furthermore, it allows you to send these values via SMS.
meta platform "current";

action main() {
  $here := wall→ask_boolean("My current location?", "");
  $lat := 0;
  $lon := 0;
  if $here→equals(false) then {
    $address := wall→ask_string("Address:");
    $cap := wall→ask_string("Postal code:");
    $city := wall→ask_string("City:");
    $country := wall→ask_string("Country:");
    $myLoc := locations→search_location($address, $cap, $city, $country);
    $lat := $myLoc→latitude;
    $lon := $myLoc→longitude;
  }
  else {
    $lat := senses→current_location→latitude;
    $lon := senses→current_location→longitude;
  }
  wall→push_new_page;
  $textLat := "Latitude: ";
  $textLon := "Longitude: ";
  $map := maps→create_map;
  $map→set_center(locations→create_location($lat, $lon));
  $map→set_zoom(16);
  $map→add_text(locations→create_location($lat, $lon), "My location", colors→background, colors→foreground);
  $map→post_to_wall;
  $textLon := $textLon→concat($lon→to_string);
  $textLon→post_to_wall;
  $textLat := $textLat→concat($lat→to_string);
  $textLat→post_to_wall;
  $sms := wall→ask_boolean("Do you want send latitude and longitude to your friend?", "");
  if $sms→equals(true) then {
    social→send_sms("", $textLat→concat(" - ")→concat($textLon));
  }
}

