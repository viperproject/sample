meta version "v2.2,nothing";
meta name "MyMapTracker4web";
meta icon "globeas";
meta color "#ff004b49";
// MyMapTracker merkt sich Orte, Adressen und Fotos und ermöglicht so das Erstellen eines eigenen Reisetagebuchs. Die gesammelten Orte (sog. Maps) können exportiert und als Wegpunkte in Bing- oder Google Maps geladen werden.
meta platform "current,accelerometer,camera,compass,location,maps,media,contacts,orientation,search,translation,musicandsounds,webonly";

action main() {
  data→mapname := "";
  code→showloc;
}
var myloc : Location {
  // Aktuelle Position
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("Update") then {
    code→showloc;
  }
  if $item→text→equals("Hinzu") then {
    data→mypic := invalid→picture;
    code→saveloc;
  }
  if $item→text→equals("Map") then {
    wall→push_new_page;
    code→showmap;
  }
  if $item→text→equals("Menü") then {
    wall→push_new_page;
    code→showmenu;
  }
  if $item→text→equals("Alles") then {
    wall→push_new_page;
    code→clearmap;
  }
  meta private;
}

action showloc() {
  wall→clear;
  wall→set_title("Position");
  data→myloc := senses→current_location_accurate;
  if not data→myloc→is_invalid then {
    $map := maps→create_map;
    $map→add_text(data→myloc, "Hier", colors→background, colors→foreground);
    $map→set_zoom(16);
    $map→set_center(data→myloc);
    $map→post_to_wall;
    data→myadr := locations→describe_location(data→myloc);
    if data→myadr→is_invalid then {
      data→myadr := "Adresse nicht ermittelbar, bitte wiederholen";
    }
    data→myadr→post_to_wall;
    wall→clear_buttons;
    wall→add_button("add", "Hinzu");
  }
  else {
    "Position konnte nicht ermittelt werden. Bitte nochmal versuchen." →post_to_wall;
  }
  wall→add_button("refresh", "Update");
  if code→anzlocs > 0 then {
    wall→add_button("favs", "Map");
  }
  wall→add_button("feature.settings", "Menü");
  meta private;
}
var myadr : String {
  // Aktuelle Adresse
}
table points {
  type = "Table";
  fields {
    loc: Location_field
    adr: String_field
    pic: Picture_field
    mapname: String_field
  }
}
var mypic : Picture {
  // Zuletzt geschossenes Bild
}

action saveloc() {
  wall→clear;
  $p := records→points_table→add_row;
  $p→loc→set(data→myloc);
  $p→adr→set(data→myadr);
  if not data→mypic→is_invalid then {
    $p→pic→set(data→mypic);
  }
  $p→mapname→set(data→mapname);
  wall→prompt("Standort gespeichert:\n" ∥ data→myadr);
  code→showloc;
  meta private;
}

action showmap() {
  wall→clear;
  wall→clear_buttons;
  $map := maps→create_full_map;
  $ls := collections→create_location_collection;
  foreach p in records→points_table where true do {
    if $p→mapname→get→equals(data→mapname) then {
      $ls→add($p→loc→get);
      if $p→pic→get→is_invalid then {
        $map→add_text($p→loc→get, $p→adr→get, colors→background, colors→foreground);
      }
      else {
        $map→add_picture($p→loc→get, $p→pic→get, colors→background);
      }
    }
  }
  $map→add_line($ls, colors→accent, 6);
  $map→post_to_wall;
  meta private;
}

action showmenu() {
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Menü");
  $anz := code→anzlocs();
  $tb := wall→create_text_box("", 36);
  $tb := wall→create_text_box("Info über MyMapTracker", 36);
  $tb→set_background(art→bgr);
  $tb→post_to_wall;
  if $anz > 0 then {
    $tb := wall→create_text_box("Map exportieren", 36);
    $tb→set_background(art→bgr);
    $tb→post_to_wall;
    $tb := wall→create_text_box("Screenshot speichern", 36);
    $tb→set_background(art→bgr);
    $tb→post_to_wall;
    $tb := wall→create_text_box("Map speichern unter...", 36);
    $tb→set_background(art→bgr);
    $tb→post_to_wall;
  }
  $tb := wall→create_text_box("Map laden", 36);
  $tb→set_background(art→bgr);
  $tb→post_to_wall;
  if $anz > 0 then {
    $tb := wall→create_text_box("Neue Map", 36);
    $tb→set_background(art→bgr);
    $tb→post_to_wall;
    $tb := wall→create_text_box("Orte bearbeiten", 36);
    $tb→set_background(art→bgr);
    $tb→post_to_wall;
  }
  if not data→mapname→is_empty then {
    $s := "Aktuelle Map: " ∥ data→mapname;
    $s→post_to_wall;
  }
  meta private;
}
var bgr : Color {
  is\_resource = true;
  url = "#ff0d5159";
}

action list() {
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Orte");
  foreach p in records→points_table where true do {
    if $p→mapname→get→equals(data→mapname) then {
      $tb := wall→create_text_box($p→adr→get, 26);
      if not $p→pic→get→is_invalid then {
        $tb→set_icon($p→pic→get);
      }
      $tb→set_background(art→bgr);
      $tb→post_to_wall;
    }
  }
  if not data→mapname→is_empty then {
    $s := "Aktuelle Map: " ∥ data→mapname;
    $s→post_to_wall;
  }
  wall→add_button("cancel", "Alles");
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  $ok := false;
  if $item→text→equals("Orte bearbeiten") then {
    $ok := true;
    wall→push_new_page;
    code→list;
  }
  if $item→text→equals("Löschen") then {
    $ok := true;
    code→deladr;
    wall→pop_page;
    code→list;
  }
  if $item→text→equals("Text ändern") then {
    $ok := true;
    code→changeadr;
    wall→pop_page;
    code→list;
  }
  if $item→text→equals("Bild auswählen") then {
    $ok := true;
    $pic := media→choose_picture;
    if not $pic→is_invalid then {
      code→setpic($pic);
      wall→pop_page;
      code→list;
    }
  }
  if $item→text→equals("Bild entfernen") then {
    $ok := true;
    code→removepic;
    wall→pop_page;
    code→list;
  }
  if $item→text→equals("Map exportieren") then {
    $ok := true;
    code→submit;
  }
  if $item→text→equals("Screenshot speichern") then {
    $ok := true;
    code→mapshot;
  }
  if $item→text→equals("Map speichern unter...") then {
    $ok := true;
    code→savemap;
  }
  if $item→text→equals("Map laden") then {
    $ok := true;
    code→loadmap;
  }
  if $item→text→equals("Neue Map") then {
    $ok := true;
    code→newmap;
  }
  if $item→text→equals("Info über MyMapTracker") then {
    $ok := true;
    wall→push_new_page;
    code→showabout;
  }
  if $item→text→equals("www.dschlieckmann.de") then {
    $ok := true;
    web→browse("http://www.dschlieckmann.de");
  }
  if $item→text→equals("info@dschlieckmann.de") then {
    $ok := true;
    social→send_email("info@dschlieckmann.de", "MyMapTracker", "");
  }
  if $item→text→starts_with("Map: ") then {
    $ok := true;
    data→mapname := $item→text→substring(5, $item→text→count - 5);
    wall→pop_page;
    wall→pop_page;
    code→showloc;
  }
  if not $ok then {
    wall→push_new_page;
    code→adrmenu($item→text);
  }
  meta private;
}

action adrmenu(adr: String) {
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Bearbeiten");
  data→seladr := $adr;
  $pic := invalid→picture;
  foreach p in records→points_table where true do {
    if $p→adr→get→equals($adr) then {
      $pic := $p→pic→get;
    }
  }
  $tb := wall→create_text_box("Löschen", 36);
  $tb→set_background(art→bgr);
  $tb→post_to_wall;
  if not $pic→is_invalid then {
    $tb := wall→create_text_box("Bild entfernen", 36);
    $tb→set_background(art→bgr);
    $tb→post_to_wall;
  }
  $tb := wall→create_text_box("Bild auswählen", 36);
  $tb→set_background(art→bgr);
  $tb→post_to_wall;
  $tb := wall→create_text_box("Text ändern", 36);
  $tb→set_background(art→bgr);
  $tb→post_to_wall;
  $s := $adr ∥ ":\n";
  $s→post_to_wall;
  meta private;
}

action deladr() {
  foreach p in records→points_table where true do {
    if $p→adr→get→equals(data→seladr) then {
      $p→delete_row;
    }
  }
  meta private;
}
var seladr : String {
}

action changeadr() {
  $s := wall→ask_string("Neuer Text:");
  if $s→count > 0 then {
    foreach p in records→points_table where true do {
      if $p→adr→get→equals(data→seladr) then {
        $p→adr→set($s);
      }
    }
  }
  meta private;
}

action setpic(pic: Picture) {
  foreach p in records→points_table where true do {
    if $p→adr→get→equals(data→seladr) then {
      $p→pic→set($pic);
    }
  }
  meta private;
}

action removepic() {
  foreach p in records→points_table where true do {
    if $p→adr→get→equals(data→seladr) then {
      $p→pic→clear;
    }
  }
  meta private;
}

action submit() {
  $s := "";
  $s := $s ∥ "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>";
  $s := $s ∥ "<rss version=\"2.0\" xmlns:geo=\"http://www.w3.org/2003/01/geo/wgs84_pos#\" xmlns:georss=\"http://www.georss.org/georss\" xmlns:gml=\"http://www.opengis.net/gml\" xmlns:mappoint=\"http://virtualearth.msn.com/apis/annotate#\">";
  $s := $s ∥ "<channel>";
  $s := $s ∥ "<title>" ∥ data→mapname ∥ "</title>";
  foreach p in records→points_table where true do {
    $s := $s ∥ "<item><title>" ∥ $p→adr→get ∥ "</title>";
    $s := $s ∥ "<geo:lat>" ∥ $p→loc→get→latitude→to_string ∥ "</geo:lat>";
    $s := $s ∥ "<geo:long>" ∥ $p→loc→get→longitude→to_string ∥ "</geo:long>";
    $s := $s ∥ "</item>";
  }
  $s := $s ∥ "</channel></rss>";
  social→send_email("", "MyMapTracker, " ∥ data→mapname, $s);
  meta private;
}

action mapshot() {
  wall→push_new_page;
  wall→clear;
  wall→clear_buttons;
  wall→prompt("Hiermit wird ein Screenshot der Map erstellt, sobald das Gerät dreimal vibriert hat.");
  code→showmap;
  time→sleep(1);
  for 0 ≤ i < 3 do {
    phone→vibrate(0.1);
    time→sleep(1);
  }
  $pic := wall→screenshot;
  $pic→save_to_library;
  $s := wall→ask_string("Wenn Sie das Bild auf Facebook teilen möchten, geben Sie einen Text ein. Bei leerer Eingabe wird das Bild nicht geteilt.");
  if not $s→is_empty then {
    $pic→share("facebook", $s);
  }
  wall→pop_page;
  meta private;
}
var mapname : String {
}

action savemap() {
  wall→push_new_page;
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Map speichern");
  $s := wall→ask_string("Wie soll die Map heißen?");
  if not $s→is_empty then {
    foreach p in records→points_table where true do {
      if $p→mapname→get→equals(data→mapname) then {
        $p→mapname→set($s);
      }
    }
    data→mapname := $s;
    wall→prompt("Map gespeichert unter " ∥ $s);
  }
  wall→pop_page;
  code→showmenu;
  meta private;
}

action loadmap() {
  wall→push_new_page;
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Map laden");
  $merk := "";
  $tb := wall→create_text_box("", 32);
  foreach p in records→points_table where true do {
    if not $p→mapname→get→is_empty and not $p→mapname→get→equals($merk) then {
      $merk := $p→mapname→get;
      $tb := wall→create_text_box("Map: " ∥ $p→mapname→get, 36);
      $tb→set_background(art→bgr);
      $tb→post_to_wall;
    }
  }
  meta private;
}

action newmap() {
  $machen := true;
  if not data→mapname→is_empty then {
    $machen := wall→ask_boolean("Jetzt neue Map beginnen und die geladene schließen?", data→mapname);
  }
  if $machen then {
    data→mapname := "";
    foreach p in records→points_table where true do {
      if $p→mapname→get→is_empty then {
        $p→delete_row;
      }
    }
    wall→pop_page;
    code→showloc;
  }
  else {
    code→showmenu;
  }
  meta private;
}

action clearmap() {
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Map löschen");
  $b := wall→ask_boolean("Hiermit werden sämtliche Orte von der Map entfernt. Sicher?", data→mapname);
  if $b then {
    foreach p in records→points_table where true do {
      if $p→mapname→get→equals(data→mapname) then {
        $p→delete_row;
      }
    }
    data→mapname := "";
    wall→pop_page;
    wall→pop_page;
    code→showmenu;
  }
  else {
    wall→pop_page;
  }
  meta private;
}

action anzlocs() returns x1: Number {
  $x1 := 0;
  foreach p in records→points_table where true do {
    if $p→mapname→get→equals(data→mapname) then {
      $x1 := $x1 + 1;
    }
  }
  meta private;
}

action showabout() {
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Info");
  $tb := wall→create_text_box("www.dschlieckmann.de", 36);
  $tb→set_background(art→bgr);
  $tb→post_to_wall;
  $tb := wall→create_text_box("info@dschlieckmann.de", 36);
  $tb→set_background(art→bgr);
  $tb→post_to_wall;
  "MyMapTracker\nVersion 1.0\n\nCopyright (c) 2012 - 2013, Daniel Schlieckmann\n\n" →post_to_wall;
  meta private;
}

