meta version "v2.2,nothing";
meta name "KfzPilot mobile";
meta icon "piechart";
meta color "#ff007fff";
// Die mobile KfzPilot-App für Autohäuser
meta platform "accelerometer,camera,location,maps";

action main() {
  wall→set_reversed(true);
  if data→lizenz→is_empty or data→plz→is_empty then {
    code→einstellungen;
  }
  else {
    code→leseeinstellungen;
    code→startpage;
  }
}

action input\_manuell() returns eingabe: String {
  wall→clear;
  wall→set_title("Kfz-Suche");
  $s := wall→ask_string("Bitte Suchbegriff(e) eingeben:");
  $eingabe := $s;
  meta private;
}

action startpage() {
  data→indid := 0;
  wall→clear;
  wall→clear_buttons;
  wall→display_search(false);
  wall→set_title("Hauptmenü");
  wall→add_button("feature.settings", "Einstell");
  $tb := wall→create_text_box("Fahrzeug suchen", 46);
  $tb→set_background(art→hellblau);
  $tb→post_to_wall;
  $tb := wall→create_text_box("Abfotografieren", 46);
  $tb→set_background(art→hellblau);
  $tb→post_to_wall;
  $tb := wall→create_text_box("Zuletzt aufgerufen", 46);
  $tb→set_background(art→hellblau);
  $tb→post_to_wall;
  wall→create_text_box("", 26)→post_to_wall;
  art→attribut→post_to_wall;
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("Einstell") then {
    wall→push_new_page;
    code→einstellungen;
  }
  if $item→text→equals("Start") then {
    code→startpage;
  }
  if $item→text→equals("Vormerk.") then {
    code→kfzreser;
    code→startpage;
  }
  if $item→text→equals("Löschen") then {
    records→kfzhistorie_table→clear;
    code→kfzhist;
  }
  meta private;
}
var servername : String {
  // Name des Servers, auf dem KfzPilot installiert ist.
}
var cnnstr : String {
  // Connectionstring für Server
}

event tap_wall_TextBox(item: TextBox) {
  $suchtext := "";
  $aktion := "x";
  if $item→text→equals("Fahrzeug suchen") then {
    $aktion := "suche";
    wall→clear_buttons;
    wall→display_search(false);
    $suchtext := code→input\_manuell();
  }
  else {
    if $item→text→equals("Abfotografieren") then {
      $aktion := "suche";
      wall→clear_buttons;
      wall→display_search(false);
      $suchtext := code→input\_kamera;
    }
    else {
      if $item→text→contains("ID: ") then {
        $aktion := "kfzdetails";
        wall→clear_buttons;
        wall→display_search(false);
        $x := $item→text→index_of("ID: ", 0);
        $s := $item→text→substring($x + 4, $item→text→count - $x - 4);
        code→addhist($s→to_number, $item→text);
        code→kfzdetails($s→to_number);
      }
      else {
        if $item→text→equals("Zuletzt aufgerufen") then {
          $aktion := "historie";
          wall→push_new_page;
          code→kfzhist;
        }
      }
    }
  }
  // Einstellungen
  if $item→text→equals("Lizenznummer") then {
    wall→clear;
    data→lizenz := wall→ask_string("Lizenznummer:");
    code→einstellungen;
  }
  else {
    if $item→text→equals("PLZ") then {
      wall→clear;
      data→plz := wall→ask_string("PLZ:");
      code→einstellungen;
    }
  }
  if $aktion→equals("suche") then {
    if not $suchtext→is_empty then {
      code→kfzliste($suchtext);
    }
    else {
      code→startpage;
    }
  }
  meta private;
}
var hellblau : Color {
  is\_resource = true;
  url = "#ff3eabbc";
}
var indid : Number {
  // Aktuelles Fahrzeug
}

action kfzreser() {
  wall→clear_buttons;
  "Fahrzeug wird reserviert..." →post_to_wall;
  $res := code→webrequest("KfzReser", "IndID", data→indid→to_string);
  if $res→equals("OK") then {
    wall→set_title("Fahrzeugangebot");
    $b := wall→ask_boolean("Möchten Sie ein Angebot mit diesem Fahrzeug in KfzPilot erzeugen?", "Abfrage");
    if $b then {
      wall→clear;
      "Fahrzeugangebot wird erzeugt..." →post_to_wall;
      $res := code→webrequest("KfzAngebot", "IndID", data→indid→to_string);
      if not $res→to_lower_case→starts_with("fehler") and not $res→to_lower_case→starts_with("hinweis") then {
        wall→prompt("Fahrzeugangebot " ∥ $res ∥ " wurde erzeugt.");
      }
      else {
        wall→prompt($res);
      }
    }
  }
  else {
    wall→prompt($res);
  }
  meta private;
}

action input\_kamera() returns eingabe: String {
  $suchtext := "";
  wall→clear;
  $pic := senses→take_camera_picture;
  if not $pic→is_invalid then {
    "Bild wird analysiert..." →post_to_wall;
    $suchtext := languages→picture_to_text("de", $pic);
  }
  $eingabe := $suchtext;
  meta private;
}
var attribut : Picture {
  is\_resource = true;
  url = "http://www.attribut.info/templates/attRiBut/images/attRiBut_Logo.png";
}

action getsoapheader() returns xml: String {
  $s := "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Header>\n    <LoginType xmlns=\"http://KPfWService.org/\">\n      <ConnectionString>$CNNSTR</ConnectionString>\n      <Filiale>0</Filiale>\n      <Lizenz>93203</Lizenz>\n      <PLZ>22941</PLZ>\n      <KPfWUser>attribut</KPfWUser>\n      <ZentralVerz>string</ZentralVerz>\n    </LoginType>\n  </soap:Header>";
  $s := $s→replace("$CNNSTR", data→cnnstr);
  $xml := $s;
  meta private;
}

action kfzliste(begriff: String) {
  wall→push_new_page;
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Suchvorgang");
  "Fahrzeugsuche läuft..." →post_to_wall;
  $s := code→webrequest("KfzSuche", "Suchbegriff", $begriff);
  wall→clear;
  wall→set_title("Kfz-Liste");
  if $s→to_lower_case→starts_with("fehler") or $s→to_lower_case→starts_with("hinweis") then {
    wall→prompt($s);
    code→startpage;
  }
  else {
    $ds := $s→split("|");
    foreach s1 in $ds where true do {
      $tb := wall→create_text_box($s1→replace(";", "\n"), 19);
      $tb→set_background(art→hellblau);
      $tb→post_to_wall;
    }
    wall→display_search(true);
    wall→add_button("back", "Start");
  }
  meta private;
}

action webrequest(soapaction: String, paramname: String, paramwert: String) returns res: String {
  $s := code→getsoapheader();
  $s := $s ∥ "<soap:Body>\n    <$SOAPACTION xmlns=\"http://KPfWService.org/\">\n      <$PARAMNAME>$PARAMWERT</$PARAMNAME>\n    </$SOAPACTION>\n  </soap:Body>\n</soap:Envelope>";
  $s := $s→replace("$SOAPACTION", $soapaction);
  $s := $s→replace("$PARAMNAME", $paramname);
  $s := $s→replace("$PARAMWERT", $paramwert);
  $xml := web→xml($s);
  $request := web→create_request(data→servername);
  $request→set_content_as_xml($xml);
  $request→set_method("post");
  $request→set_header("SOAPAction", "http://KPfWService.org/" ∥ $soapaction);
  $request→set_header("content-type", "text/xml;charset=utf-8");
  $response := $request→send;
  if $response→status_code = 200 then {
    $xml1 := web→xml($response→content);
    $s1 := $xml1→to_string;
    $anfang := $s1→index_of("<" ∥ $soapaction ∥ "Result>", 0);
    $ende := $s1→index_of("</" ∥ $soapaction ∥ "Result>", 0);
    if $anfang ≥ 0 and $ende ≥ 0 then {
      $daten := $s1→substring($anfang + 8 + $soapaction→count, $ende - $anfang - 8 - $soapaction→count);
      $res := $daten;
    }
    else {
      $res := "";
    }
  }
  else {
    $res := "FEHLER: Abruf fehlgeschlagen. Statuscode " ∥ $response→status_code;
  }
  meta private;
}

action kfzdetails(indid: Number) {
  data→indid := $indid;
  wall→push_new_page;
  wall→set_reversed(true);
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Fahrzeug");
  $res := code→webrequest("KfzDaten", "IndID", $indid→to_string);
  if $res→to_lower_case→starts_with("fehler") or $res→to_lower_case→starts_with("hinweis") then {
    wall→prompt($res);
    code→startpage;
  }
  else {
    $zeilen := $res→split("|");
    foreach s in $zeilen where true do {
      $txt := $s→replace(";", "\n");
      $tb := wall→create_text_box("", 19);
      $tb→set_background(colors→black);
      if $s→starts_with("@ ") then {
        $tb→set_font_size(32);
        $tb→set_foreground(art→hellblau);
        $tb→set_text($txt→substring(2, $txt→count - 2));
      }
      else {
        $tb→set_foreground(colors→light_gray);
        $tb→set_text($txt);
      }
      $tb→post_to_wall;
    }
    $tb1 := wall→create_text_box(" ", 19);
    $tb1→set_background(colors→black);
    $tb1→post_to_wall;
    wall→add_button("back", "Start");
    wall→add_button("download", "Vormerk.");
  }
  meta private;
}
var lizenz : String {
  // Lizenzschlüssel
}
var plz : String {
}

action einstellungen() {
  wall→clear;
  wall→set_title("Einstellungen");
  wall→clear_buttons;
  wall→set_reversed(true);
  code→leseeinstellungen;
  $tb := wall→create_text_box("Lizenznummer", 46);
  $tb→set_background(art→hellblau);
  $tb→post_to_wall;
  $s := "Aktueller Wert: " ∥ data→lizenz;
  $s→post_to_wall;
  $tb := wall→create_text_box("PLZ", 46);
  $tb→set_background(art→hellblau);
  $tb→post_to_wall;
  $s := "Aktueller Wert: " ∥ data→plz;
  $s→post_to_wall;
  $status := wall→create_text_box("", 19);
  if data→cnnstr→is_empty then {
    $status→set_text("Die Informationen sind noch unvollständig.");
    $status→set_foreground(colors→red);
  }
  else {
    $status→set_text("Die Einstellungen sind in Ordnung :-)");
    $status→set_foreground(colors→green);
  }
  $status→post_to_wall;
  meta private;
}

action leseeinstellungen() {
  data→cnnstr := "";
  if not data→lizenz→is_empty and not data→plz→is_empty then {
    "Einstellungen werden geladen..." →post_to_wall;
    $s := "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Header>\n    <CredentialType xmlns=\"http://KPfWLiz.org/\">\n      <Lizenz>$LIZENZ</Lizenz>\n      <PLZ>$PLZ</PLZ>\n      <Password>IngaBastian</Password>\n    </CredentialType>\n  </soap:Header>\n  <soap:Body>\n    <GetConnectParams xmlns=\"http://KPfWLiz.org/\" />\n  </soap:Body>\n</soap:Envelope>";
    $s := $s→replace("$LIZENZ", data→lizenz);
    $s := $s→replace("$PLZ", data→plz);
    $xml := web→xml($s);
    $request := web→create_request("http://attributsql.dynalias.net/KPfWLiz/Service.asmx");
    $request→set_content_as_xml($xml);
    $request→set_method("post");
    $request→set_header("SOAPAction", "http://KPfWLiz.org/GetConnectParams");
    $request→set_header("content-type", "text/xml;charset=utf-8");
    $response := $request→send;
    if $response→status_code = 200 then {
      $xml1 := web→xml($response→content);
      $s1 := $xml1→to_string;
      $anfang := $s1→index_of("<GetConnectParamsResult>", 0);
      $ende := $s1→index_of("</GetConnectParamsResult>", 0);
      if $anfang ≥ 0 and $ende ≥ 0 then {
        $daten := $s1→substring($anfang + 24, $ende - $anfang - 24);
        $strings := $daten→split("|");
        data→servername := $strings→at(0);
        data→cnnstr := $strings→at(1);
      }
    }
    else {
      wall→prompt("Problem beim Lesen der Einstellungen. Bitte überprüfen Sie die Korrektheit Ihres Lizenzschlüssels und Ihrer PLZ. Statuscode " ∥ $response→status_code);
    }
  }
  wall→clear;
  meta private;
}
table kfzhistorie {
  type = "Table";
  fields {
    indid: Number_field
    bezeichnung: String_field
  }
}

action kfzhist() {
  wall→clear;
  wall→clear_buttons;
  wall→set_title("Historie");
  if records→kfzhistorie_table→count > 0 then {
    wall→display_search(true);
    wall→add_button("delete", "Löschen");
  }
  else {
    wall→display_search(false);
    "Keine Einträge vorhanden." →post_to_wall;
  }
  foreach k in records→kfzhistorie_table where true do {
    $tb := wall→create_text_box($k→bezeichnung→get, 19);
    $tb→set_background(art→hellblau);
    $tb→post_to_wall;
  }
  meta private;
}

action addhist(indid: Number, bezeichnung: String) {
  $gefunden := false;
  foreach k in records→kfzhistorie_table where true do {
    if $k→indid→get = $indid then {
      $gefunden := true;
    }
  }
  if not $gefunden then {
    $k1 := records→kfzhistorie_table→add_row;
    $k1→indid→set($indid);
    $k1→bezeichnung→set($bezeichnung);
  }
  meta private;
}

