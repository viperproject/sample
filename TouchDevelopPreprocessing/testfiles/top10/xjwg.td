meta version "v2.2,nothing";
meta name "Seattle traffic";
meta icon "map";
meta color "#ff008000";
// Opens the Map application to the right location and zoom level to see the bridges, i-5 and 405.  Pin it to your home screen and you can check your commute with one tap. Please note- you may need to "show traffic" if it is not already enabled.
meta platform "current";

action main() {
  $tb := wall→create_text_box("Opening Map", 26);
  $tb→post_to_wall;
  code→map;
}

action map() {
  $map := maps→create_full_map;
  $center := locations→create_location(47.6, 0 - 122.25);
  $map→set_zoom(11.5);
  $map→set_center($center);
  maps→open_map($center, "", .58);
  ... ;
}

