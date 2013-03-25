meta version "v2.2,nothing";
meta name "Cash Matrix 5m";
meta icon "8ball";
meta color "#ff66ff00";
// Mobiler Client für das Haushaltsbuch Cash Matrix 5 - Registrierung erforderlich.
meta platform "current";

action main() {
  if code→init then {
    code→frmMain;
  }
}

action EintragNeuSave(loc\_bez: String, loc\_bet: String, loc\_dat: String, loc\_kat: Number, loc\_lux: Number) {
  $tmp\_row := records→MyData_table→add_row;
  $tmp\_row→bez→set($loc\_bez);
  $tmp\_row→bet→set($loc\_bet);
  $tmp\_row→dat→set($loc\_dat);
  $tmp\_row→kat→set(data→kats→at($loc\_kat));
  $tmp\_row→lux→set($loc\_lux);
  code→frmMain;
  meta private;
}
table MyData {
  type = "Table";
  fields {
    bet: String_field
    bez: String_field
    dat: String_field
    kat: String_field
    lux: Number_field
  }
}

action init() returns b1: Boolean {
  code→initGfx;
  if code→initLiveID then {
    code→EigenLoad;
    $b1 := true;
  }
  else {
    "Ihre Anmeldung ist fehlgeschlagen." →post_to_wall;
    $b1 := false;
  }
  meta private;
}
var kats : String_Collection {
}

action EintragNeu() {
  wall→clear;
  wall→set_reversed(true);
  wall→set_title("Neuer Eintrag");
  wall→clear_buttons;
  $tmp\_bez := "";
  while $tmp\_bez→count < 3 do {
    "Geben Sie mindestens 3 Zeichen als Bezeichnung ein." →post_to_wall;
    $tmp\_bez := wall→ask_string("Bezeichnung");
    wall→clear;
  }
  "Geben Sie einen Betrag ein (Format: ##.##). Geben Sie bei ganzen Beträgen keinen Punkt an (##)." →post_to_wall;
  $tmp\_bet := wall→ask_string("Betrag");
  if $tmp\_bet→contains(",") then {
    if $tmp\_bet→contains(".") then {
      $tmp\_bet := $tmp\_bet→replace(".", "");
    }
    $tmp\_bet := $tmp\_bet→replace(",", ".");
  }
  if $tmp\_bet→contains(".") then {
    skip;
  }
  else {
    $tmp\_bet := $tmp\_bet ∥ ".00";
  }
  wall→clear;
  "Geben Sie das Datum ein. Verwenden Sie lediglich den Tag für den aktuellen Monat." →post_to_wall;
  $tmp\_dat := wall→ask_string("Datum");
  if $tmp\_dat→count < 3 then {
    $s := "." ∥ time→today→month ∥ "." ∥ time→today→year;
    $tmp\_dat := $tmp\_dat ∥ $s;
  }
  wall→clear;
  $tmp\_kat := wall→pick_string("Kategorie", "", data→kats);
  wall→clear;
  $tmp\_lux := wall→pick_string("Klassifikation", "", data→lux);
  wall→clear;
  code→EintragNeuSave($tmp\_bez, $tmp\_bet, $tmp\_dat, $tmp\_kat, $tmp\_lux);
  code→frmMain;
  meta private;
}

action initGfx() {
  wall→set_title("Willkommen!");
  skip;
  $color := colors→purple;
  wall→set_background($color);
  meta private;
}

action EigenLoad() {
  $request := web→create_request("http://www.cash-Matrix.de/gadget/td-kat.php?liv=" ∥ data→LiveID);
  $s := $request→send→content;
  data→kats := $s→split(",");
  foreach e in $s where true do {
    skip;
  }
  data→lux := collections→create_string_collection;
  data→lux→add("Grundbedarf");
  data→lux→add("Erw. Grundbedarf");
  data→lux→add("Alltag");
  data→lux→add("Erw. Alltag");
  data→lux→add("Luxus");
  meta private;
}

action initLiveID() returns b1: Boolean {
  wall→set_subtitle("nicht angemeldet");
  wall→set_reversed(true);
  wall→clear_buttons;
  if web→is_connected then {
    $tmp\_liveid := data→LiveID;
    while $tmp\_liveid→is_empty do {
      "Sie sind derzeit nicht angemeldet. Bitte beachten Sie, dass Sie für die Nutzung dieser App eine Cash Matrix Live-ID benötigen. Diese erhalten Sie, wenn Sie die Vollversion der Cash Matrix 5 erwerben." →post_to_wall;
      $tmp\_liveid := wall→ask_string("Ihre Live-ID");
      wall→clear;
    }
    $tmp\_pass := "";
    while $tmp\_pass→is_empty do {
      $tmp\_pass := wall→ask_string("Ihr Passwort");
      wall→clear;
    }
    "Live-ID wird überprüft..." →post_to_wall;
    $s := "liv=" ∥ $tmp\_liveid→replace(" ", "%20") ∥ "&pas=" ∥ $tmp\_pass→replace(" ", "%20");
    $request := web→create_request("http://www.cash-matrix.de/gadget/td-auth?" ∥ $s);
    $request→set_method("post");
    $response := $request→send;
    $s2 := $response→content;
    if $s2→starts_with("1") then {
      data→LiveID := $tmp\_liveid;
      wall→set_subtitle("angemeldet als " ∥ data→LiveID);
      wall→clear;
      $b1 := true;
    }
    else {
      $b1 := false;
    }
  }
  else {
    "Ihr Gerät hat gerade keine Verbindung mit dem Internet." →post_to_wall;
    $b1 := false;
  }
  meta private;
}

action frmMain() {
  wall→clear;
  wall→set_reversed(true);
  wall→set_title("Haushaltsbuch");
  wall→clear_buttons;
  wall→add_button("add", "Neu");
  wall→add_button("folder", "Kategorien");
  if records→MyData_table→count > 0 then {
    "Erfasste Umsätze" →post_to_wall;
    $x := 0;
    $x2 := 0;
    foreach e in records→MyData_table where true do {
      $x := $x + 1;
      wall→create_text_box($x ∥ ". " ∥ $e→bez→get ∥ " (" ∥ $e→bet→get ∥ ")", 24)→post_to_wall;
      $x2 := $x2 + $e→bet→get→to_number;
    }
    if $x > 0 then {
      wall→create_text_box("Summe: " ∥ $x2, 19)→post_to_wall;
    }
    wall→add_button("sync", "Sync");
  }
  else {
    "Keine Einträge vorhanden." →post_to_wall;
    "Wählen Sie das Plus, um einen neuen Eintrag anzulegen." →post_to_wall;
  }
  wall→add_button("question", "Hilfe");
  meta private;
}
table thing {
  type = "Decorator";
  keys {
    target: TextBox
  }
  fields {
    choice: String_field
  }
}
var LiveID : String {
}

action frmKats() {
  wall→clear;
  wall→set_reversed(true);
  wall→set_title("Kategorien");
  wall→clear_buttons;
  wall→add_button("back", "zurück");
  for 0 ≤ i < data→kats→count do {
    data→kats→at($i)→post_to_wall;
  }
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→starts_with("Neu") then {
    code→EintragNeu;
  }
  else {
    if $item→text→starts_with("Kat") then {
      code→frmKats;
    }
    else {
      if $item→text→starts_with("zur") then {
        code→frmMain;
      }
      else {
        if $item→text→starts_with("Hil") then {
          code→frmHilfe;
        }
        else {
          if $item→text→starts_with("Syn") then {
            code→frmSynch;
          }
          else {
            if $item→text→starts_with("Cle") then {
              records→MyData_table→clear;
              data→LiveID := "";
              code→main;
            }
          }
        }
      }
    }
  }
  meta private;
}
var lux : String_Collection {
}

action frmHilfe() {
  wall→clear;
  wall→set_reversed(true);
  wall→set_title("Hilfe");
  wall→clear_buttons;
  wall→add_button("back", "zurück");
  wall→add_button("delete", "Clear");
  "Version 1.1 beta" →post_to_wall;
  "Die Cash Matrix 5m ist ein Client für Windows Phone 8 und Windows RT, um für das Haushaltsbuch Cash Matrix 5 (www.haushaltsbuch-cash-matrix.de) Umsätze zu erfassen." →post_to_wall;
  "Damit der Client verwendet werden kann, ist sowohl die Installation der Desktop App, als auch eine kostenpflichtige Registrierung der Vollversion erforderlich." →post_to_wall;
  meta private;
}

action frmSynch() {
  wall→clear;
  wall→set_reversed(true);
  wall→set_title("Synchronisieren...");
  wall→clear_buttons;
  wall→add_button("back", "zurück");
  if web→is_connected then {
    "Erfasste Umsätze werden übertragen..." →post_to_wall;
    $x3 := 0;
    foreach e in records→MyData_table where true do {
      $x3 := $x3 + 1;
      $tmp\_bez := $e→bez→get→replace(" ", "%20");
      $tmp\_bet := $e→bet→get;
      $tmp\_dat := $e→dat→get;
      $tmp\_kat := $e→kat→get→replace(" ", "%20");
      $tmp\_lux := $e→lux→get;
      $tmp\_str := "http://www.cash-Matrix.de/gadget/td-add.php?" ∥ "liv=" ∥ data→LiveID ∥ "&bez=" ∥ $tmp\_bez ∥ "&bet=" ∥ $tmp\_bet ∥ "&dat=" ∥ $tmp\_dat ∥ "&kat=" ∥ $tmp\_kat ∥ "&lux=" ∥ $tmp\_lux;
      $request := web→create_request($tmp\_str);
      $response := $request→send;
      $left := $response→content;
      if $left→starts_with("1") then {
        $s := $x3 ∥ ". " ∥ $e→bez→get ∥ " eingefügt.";
        $s→post_to_wall;
        $e→delete_row;
      }
      else {
        $s1 := $x3 ∥ ". " ∥ $e→bez→get ∥ ": Fehler.";
        $s1→post_to_wall;
        "fehler" →post_to_wall;
      }
    }
  }
  else {
    "Keine Verbindung mit dem Internet." →post_to_wall;
  }
  meta private;
}

