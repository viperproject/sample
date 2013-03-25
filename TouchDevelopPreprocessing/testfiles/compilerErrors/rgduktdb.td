meta version "v2.2,nothing";
meta name "Feed Manager";
meta icon "Suitcase";
meta color "#ff00008B";
//
meta platform "current";

action main() {
  code→show\_menu;
}
table feeds {
  type = "Table";
  fields {
    URL: String_field
    Bezeichnung: String_field
    LastRead: DateTime_field
  }
}

action show\_menu() {
  wall→clear;
  wall→set_title("Feed Manager");
  $wahl := - 2;
  $feedcoll := collections→create_string_collection;
  $feedcoll→add("(Neuen Feed anlegen)");
  foreach e in records→feeds_table where true do {
    $feedcoll→add($e→Bezeichnung→get);
  }
  $wahl := wall→pick_string("Menü", "Bitte wählen Sie einen Feed:", $feedcoll);
  if $wahl = 0 then {
    code→new\_feed;
  }
  else {
    code→show\_feed($feedcoll→at($wahl));
  }
  meta private;
}

action create\_test\_data() {
  $feeds1 := records→feeds_table→add_row;
  $feeds1→URL→set("http://www.attribut.info/aktuelles?format=feed&amp;type=rss");
  $feeds1→Bezeichnung→set("attRiBut Aktuelles");
  $feeds1 := records→feeds_table→add_row;
  $feeds1→URL→set("http://www.spiegel.de/index.rss");
  $feeds1→Bezeichnung→set("Spiegel");
  meta private;
}

action new\_feed() {
  wall→clear;
  wall→set_title("Neuer Feed");
  $URL := wall→ask_string("Feed-URL");
  if $URL→count > 0 then {
    $Bez := wall→ask_string("Bezeichnung");
    if $Bez→count > 0 then {
      $newfeed := records→feeds_table→add_row;
      $newfeed→URL→set($URL);
      $newfeed→Bezeichnung→set($Bez);
    }
  }
  code→show\_menu;
  meta private;
}

action show\_feed(Bezeichnung: String) {
  wall→push_new_page;
  wall→clear;
  wall→set_title($Bezeichnung);
  wall→add_button("add", "Entf.");
  foreach e in records→feeds_table where true do {
    if $e→Bezeichnung→get→equals($Bezeichnung) then {
      data→current\_feed := $Bezeichnung;
      $e→LastRead→set(time→now);
      $inhalt := web→download($e→URL→get);
      $msgs := web→feed($inhalt);
      $msgs→post_to_wall;
    }
  }
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→icon→equals("delete") then {
    $b := wall→ask_boolean("Feed entfernen", "Möchten Sie diesen Feed wirklich aus der Liste entfernen?");
    if $b then {
      foreach e in records→feeds_table where true do {
        if $e→Bezeichnung→get→equals(data→current\_feed) then {
          $e→delete_row;
        }
      }
      wall→pop_page;
    }
  }
  meta private;
}

event page_navigated_from() {
  code→show\_menu;
  meta private;
}
var current\_feed : String {
}

