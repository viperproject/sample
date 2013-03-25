meta version "v2.2,nothing";
meta name "WhatsImage";
meta icon "photos";
meta color "#ff66ff00";
//
meta platform "current";

action main() {
  $s := web→connection_type;
  if $s→equals("wifi") then {
    $s1 := wall→ask_string("Search image: ");
    web→search_images($s1)→post_to_wall;
    ... ;
  }
  else {
    wall→prompt("No WiFi.\nNo image.");
  }
}
table thing {
  type = "Table";
  fields {
    x: Number_field
  }
}

