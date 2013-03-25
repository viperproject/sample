meta version "v2.2,nothing";
meta name "Lukkari";
meta icon "threecolumn";
meta color "#ffff7518";
//
meta platform "";

action Main() {
  if data→Kurssit→is_invalid→equals(false) then {
    data→Main_tile→set_background(colors→accent);
    data→Main_tile→set_title("Lukkari");
    wall→set_background(colors→background);
    if data→Lauta→is_invalid then {
      data→Lauta := media→create_portrait_board(480, 800);
      data→Lauta→post_to_wall;
    }
    else {
      data→Lauta→post_to_wall;
    }
    wall→add_button("add", "Seuraava");
    wall→set_title("");
    // Tee tekstilaatikot:
    if data→Boksit→is_invalid then {
      data→Boksit := data→Lauta→create_sprite_set;
      for 0 ≤ i < 5 do {
        data→Boksit→add(data→Lauta→create_rectangle(480, 200));
      }
    }
    for 0 ≤ i1 < 5 do {
      data→Boksit→at($i1)→set_pos(240, 100 * $i1 + 200);
    }
    data→Boksit→at(0)→set_color(colors→from_rgb(colors→accent→R + 0.1, colors→accent→G + 0.1, colors→accent→B + 0.1));
    data→Boksit→at(1)→set_color(colors→from_rgb(colors→accent→R + 0.05, colors→accent→G + 0.05, colors→accent→B + 0.05));
    data→Boksit→at(2)→set_color(colors→accent);
    data→Boksit→at(3)→set_color(colors→from_rgb(colors→accent→R - 0.05, colors→accent→G - 0.05, colors→accent→B - 0.05));
    data→Boksit→at(4)→set_color(colors→from_rgb(colors→accent→R - 0.1, colors→accent→G - 0.1, colors→accent→B - 0.1));
    data→Boksit→at(5)→set_color(colors→background);
    if data→Tunnit→is_invalid then {
      data→Tunnit := data→Lauta→create_sprite_set;
      for 0 ≤ i2 < 4 do {
        data→Tunnit→add(data→Lauta→create_text(460, 100, 50, ""));
      }
    }
    for 0 ≤ i3 < 4 do {
      data→Tunnit→at($i3)→set_pos(70, 100 * $i3 + 212);
    }
    foreach sprite1 in data→Tunnit where true do {
      $sprite1→set_color(colors→white);
    }
    if data→Ajat→is_invalid then {
      data→Ajat := data→Lauta→create_sprite_set;
      data→Ajat→add(data→Lauta→create_text(100, 100, 25, "8.15-\n9.30"));
      data→Ajat→add(data→Lauta→create_text(100, 100, 25, "9.45-\n11.00"));
      data→Ajat→add(data→Lauta→create_text(100, 100, 25, "11.45-\n13.00"));
      data→Ajat→add(data→Lauta→create_text(100, 100, 25, "13.15-\n14.30"));
      data→Ajat→add(data→Lauta→create_text(100, 100, 25, "14.40-\n16.55"));
    }
    else {
      data→Ajat→at(0)→set_text("8.15-\n9.30");
      data→Ajat→at(1)→set_text("9.45-\n11.00");
      data→Ajat→at(2)→set_text("11.45-\n13.00");
      data→Ajat→at(3)→set_text("13.15-\n14.30");
      data→Ajat→at(4)→set_text("14.40-\n15.55");
    }
    for 0 ≤ i4 < 4 do {
      data→Ajat→at($i4)→set_pos(415, 100 * $i4 + 215);
    }
    foreach sprite in data→Ajat where true do {
      $sprite→set_color(colors→from_rgb(0.25, 0.25, 0.25));
    }
    // Päivitä kaikki päivän mukaan:
    if time→today→week_day = 1 then {
      wall→set_title("maanantai");
      data→Tunnit→at(0)→set_text(data→Kurssit→at(0));
      data→Tunnit→at(1)→set_text(data→Kurssit→at(1));
      data→Tunnit→at(2)→set_text(data→Kurssit→at(5));
      data→Tunnit→at(3)→set_text(data→Kurssit→at(6));
      data→Tunnit→at(4)→set_text(data→Kurssit→at(7));
    }
    else {
      if time→today→week_day = 2 then {
        wall→set_title("tiistai");
        data→Tunnit→at(0)→set_text(data→Kurssit→at(2));
        data→Tunnit→at(1)→set_text(data→Kurssit→at(3));
        data→Tunnit→at(2)→set_text(data→Kurssit→at(4));
        data→Tunnit→at(3)→set_text(data→Kurssit→at(1));
        data→Tunnit→at(4)→set_text(data→Kurssit→at(0));
      }
      else {
        if time→today→week_day = 3 then {
          wall→set_title("keskiviikko");
          data→Tunnit→at(0)→set_text(data→Kurssit→at(4));
          data→Tunnit→at(1)→set_text(data→Kurssit→at(3));
          data→Tunnit→at(2)→set_text(data→Kurssit→at(2));
          data→Tunnit→at(3)→set_text(data→Kurssit→at(5));
          data→Tunnit→at(4)→set_text(data→Kurssit→at(6));
        }
        else {
          if time→today→week_day = 4 then {
            wall→set_title("torstai");
            data→Tunnit→at(0)→set_text(data→Kurssit→at(0));
            data→Tunnit→at(1)→set_text(data→Kurssit→at(1));
            data→Tunnit→at(2)→set_text(data→Kurssit→at(5));
            data→Tunnit→at(3)→set_text(data→Kurssit→at(6));
            data→Tunnit→at(4)→set_text(data→Kurssit→at(7));
          }
          else {
            if time→today→week_day = 5 then {
              wall→set_title("perjantai");
              data→Tunnit→at(0)→set_text(data→Kurssit→at(7));
              data→Tunnit→at(1)→set_text(data→Kurssit→at(4));
              data→Tunnit→at(2)→set_text(data→Kurssit→at(2));
              data→Tunnit→at(3)→set_text(data→Kurssit→at(3));
              data→Tunnit→at(4)→set_text("");
            }
            else {
              if time→today→week_day = 6 then {
                wall→set_title("lauantai");
                foreach sprite2 in data→Tunnit where true do {
                  $sprite2→set_text("");
                }
                foreach sprite3 in data→Boksit where true do {
                  $sprite3→set_color(colors→background);
                }
                foreach sprite4 in data→Ajat where true do {
                  $sprite4→set_text("");
                }
              }
              else {
                if time→today→week_day = 7 then {
                  wall→set_title("sunnuntai");
                  foreach sprite5 in data→Tunnit where true do {
                    $sprite5→set_text("");
                  }
                  foreach sprite6 in data→Boksit where true do {
                    $sprite6→set_color(colors→background);
                  }
                  foreach sprite7 in data→Ajat where true do {
                    $sprite7→set_text("");
                  }
                }
              }
            }
          }
        }
      }
    }
    while true do {
      skip;
    }
  }
  else {
    "Aloita kirjoittamalla kurssisi asetuksissa" →post_to_wall;
    while true do {
      skip;
    }
  }
}

action Asetukset() {
  if data→Kurssit→is_invalid then {
    data→Kurssit := collections→create_string_collection;
  }
  else {
    data→Kurssit→clear;
  }
  data→Kurssit→add(wall→ask_string("1. Koodin aine:")→to_upper_case);
  data→Kurssit→add(wall→ask_string("2. Koodin aine:")→to_upper_case);
  data→Kurssit→add(wall→ask_string("3. Koodin aine:")→to_upper_case);
  data→Kurssit→add(wall→ask_string("4. Koodin aine:")→to_upper_case);
  data→Kurssit→add(wall→ask_string("5. Koodin aine:")→to_upper_case);
  data→Kurssit→add(wall→ask_string("6. Koodin aine:")→to_upper_case);
  data→Kurssit→add(wall→ask_string("7. Koodin aine:")→to_upper_case);
  data→Kurssit→add(wall→ask_string("8. Koodin aine:")→to_upper_case);
  wall→clear;
  wall→set_title("Valmis!");
}
var Kurssit : String_Collection {
}
var Lauta : Board {
}
var Ajat : Sprite_Set {
}
var Main_tile : Tile {
  readonly = true;
}
var Boksit : Sprite_Set {
}
var Tunnit : Sprite_Set {
}

action Reset() {
  data→Boksit := invalid→sprite_set;
  data→Ajat := invalid→sprite_set;
  data→Kurssit := invalid→string_collection;
  data→Tunnit := invalid→sprite_set;
}
var Asetuksiin : Page_Button {
}

event tap_wall_Page_Button(item: Page_Button) {
  if data→Viikonp\u00E4iv\u00E4 < 6 then {
    data→Viikonp\u00E4iv\u00E4 := data→Viikonp\u00E4iv\u00E4 - 1;
  }
  meta private;
}
var Viikonp\u00E4iv\u00E4 : Number {
}
var Eti\u00E4pp\u00E4in : Page_Button {
}

