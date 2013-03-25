meta version "v2.2,nothing";
meta name "Expatriation";
meta icon "ticket";
meta color "#ff007fff";
// CREATE BY SLIM
meta platform "";

action main() {
  code→homeDesign;
  skip;
}

action homeDesign() {
  wall→set_title("Expatriation ");
  wall→set_background(colors→random);
  wall→set_subtitle("Accueil");
  $s := "";
  $s1 := wall→ask_string("Nom");
  $s2 := wall→ask_string("Prenom");
  $tb := wall→create_text_box("T", 19);
  $tb→post_to_wall;
  $strings := collections→create_string_collection;
  $strings→add("Slim");
  $strings→add("Chany");
  $strings→add("Francoise");
  $strings→post_to_wall;
  wall→add_button("add", "Test")→post_to_wall;
  wall→add_button("delete", "");
  $p := records→personne_table→add_row;
  $p→nom→set($s1);
  $p→prenom→set($s2);
  ... ;
  $s3 := wall→ask_string("");
  $s4 := "";
  $s5 := "Acd" ∥ web→url_encode($s3);
  $json := web→json(web→download($s4));
  $json1 := $json→field("Questions");
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→icon→equals("add") then {
    wall→clear_buttons;
    code→afficher;
  }
  meta private;
}
table personne {
  type = "Table";
  fields {
    nom: String_field
    prenom: String_field
  }
}

action afficher() {
  wall→clear;
  foreach p in records→personne_table where true do {
    $s := $p→prenom→get;
    $s := $s→concat(" ")→concat($p→nom→get);
    $s→post_to_wall;
  }
  meta private;
}

