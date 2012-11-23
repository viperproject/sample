meta version "v2.2,nothing";
meta name "WiFi/3G Swap";
meta icon "signal";
meta color "#ff008080";
// Change WiFi and Cellular wireless settings from one place.
meta platform "current";

action main() {
  web→open_connection_settings("Wifi");
  web→open_connection_settings("Cellular");
  // Save connection string
  $s := web→connection_type;
  if $s→equals("wifi") then {
    data→main_tile→set_icon(media→large_icon("signal"));
    data→main_tile→set_background(colors→accent);
    data→main_tile→set_back_icon(media→large_icon("wifi"));
    data→main_tile→set_back_title(web→connection_name);
    data→main_tile→back_icon→post_to_wall;
    web→connection_name→post_to_wall;
    "Connected to WiFi" →post_to_wall;
  }
  if $s→equals("mobile") then {
    data→main_tile→set_icon(media→large_icon("signal"));
    data→main_tile→set_background(colors→accent);
    data→main_tile→set_back_icon(media→large_icon("signal"));
    data→main_tile→set_back_title("3G Connected");
    data→main_tile→back_icon→post_to_wall;
    "Connected to 3G" →post_to_wall;
  }
  if $s→equals("none") then {
    data→main_tile→set_background(colors→gray);
    data→main_tile→set_icon(media→large_icon("signalalt"));
    data→main_tile→set_back_icon(media→large_icon("signalalt"));
    data→main_tile→set_back_title("No Connection");
    data→main_tile→back_icon→post_to_wall;
    "No Connection" →post_to_wall;
  }
}
var main_tile : Tile {
  readonly = true;
}

