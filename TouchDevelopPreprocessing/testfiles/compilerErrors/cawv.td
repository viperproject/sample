meta version "v2.2,nothing";
meta name "Wifi, Bluetooth & Co.";
meta icon "airplane";
meta color "#ffff0038";
// Shortcut to Connection Settings V1.2
// added Tiles to see last Connection State in StartMenu.
// Pin App to StartMenu.
meta platform "current";

action main() {
  // WiFi, Bluetooth & Co V1.2
  // V1.2
  // added Tiles to show last
  // Connectionstate in StartMenu.
  data→version := 1.2;
  data→board := media→create_board(640);
  data→board→post_to_wall;
  code→createTabs;
  code→createLables;
  code→createLeds;
  code→switchLeds\u0026Tiles;
}
var board : Board {
}

action createTabs() {
  data→tab_set := data→board→create_sprite_set;
  //
  code→createTab("wifi", colors→red, "wifi", 120, 120);
  //
  code→createTab("bluetooth", colors→blue, "bolt", 320, 120);
  //
  code→createTab("cellular", colors→purple, "mobilephone", 120, 320);
  //
  code→createTab("Airplanemode", colors→green, "Airplane", 320, 320);
  //
  data→board→update_on_wall;
  meta private;
}
var tab_set : Sprite_Set {
}

event tap_sprite_in_tab_set(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  $x1 := data→tab_set→index_of($sprite);
  if $x1 ≥ 6 then {
    web→open_connection_settings("airplanemode");
    $x1 := - 1;
  }
  else {
    if $x1 ≥ 4 then {
      web→open_connection_settings("cellular");
      $x1 := - 1;
    }
    else {
      if $x1 ≥ 2 then {
        web→open_connection_settings("bluetooth");
        $x1 := - 1;
      }
      else {
        if $x1 ≥ 0 then {
          web→open_connection_settings("wifi");
          $x1 := - 1;
        }
      }
    }
  }
  code→switchLeds\u0026Tiles;
  meta private;
}

action createLables() {
  $lables := data→board→create_sprite_set;
  //
  $wiFiLable := data→board→create_text(100, 20, 20, "WiFi");
  $wiFiLable→set_pos(120, 120 + 50);
  $lables→add($wiFiLable);
  //
  $bluetoothLable := data→board→create_text(100, 20, 20, "bluetooth");
  $bluetoothLable→set_pos(320, 120 + 50);
  $lables→add($bluetoothLable);
  //
  $cellularLable := data→board→create_text(100, 20, 20, "cellular");
  $cellularLable→set_pos(120, 320 + 50);
  $lables→add($cellularLable);
  //
  $airplaneLable := data→board→create_text(100, 20, 20, "Airplane");
  $airplaneLable→set_pos(320, 320 + 50);
  $lables→add($airplaneLable);
  //
  data→board→update_on_wall;
  meta private;
}

action createTab(tabName: String, tColor: Color, icon: String, x1: Number, y1: Number) {
  $square := data→board→create_rectangle(160, 160);
  $square→set_text($tabName);
  $square→set_pos($x1, $y1);
  $square→set_color($tColor→make_transparent(0.7));
  data→tab_set→add($square);
  //
  $pic := media→large_icon($icon);
  $ico := data→board→create_picture($pic);
  $ico→set_pos($x1, $y1);
  data→tab_set→add($ico);
  meta private;
}

action switchLeds\u0026Tiles() {
  $conName := "";
  if not data→board→touched then {
    $webConType := web→connection_type;
    $conName := "";
    time→sleep(3);
    if $webConType→equals("wifi") then {
      $conName := web→connection_name;
      data→led_set→at(0)→show;
      data→led_set→at(2)→hide;
      if not data→main_tile→is_invalid then {
        data→main_tile→set_content("WiFi\n" ∥ $conName);
        data→main_tile→set_back_icon(media→large_icon("wifi"));
        data→main_tile→set_background(colors→red);
      }
    }
    else {
      if $webConType→equals("mobile") then {
        $conName := web→connection_name;
        data→led_set→at(0)→hide;
        data→led_set→at(2)→show;
        if not data→main_tile→is_invalid then {
          data→main_tile→set_content("Cellular\n" ∥ $conName);
          data→main_tile→set_back_icon(media→large_icon("MobilePhone"));
          data→main_tile→set_background(colors→purple);
        }
      }
      else {
        data→led_set→at(0)→hide;
        data→led_set→at(2)→hide;
        if not data→main_tile→is_invalid then {
          data→main_tile→set_content("no Connection");
          data→main_tile→set_back_icon(media→large_icon("Delete"));
          data→main_tile→set_background(colors→accent);
        }
      }
    }
  }
  data→board→update_on_wall;
  meta private;
}

action createLeds() {
  data→led_set := data→board→create_sprite_set;
  for 0 ≤ i < 4 do {
    $led := data→board→create_ellipse(10, 10);
    $led→set_color(colors→yellow);
    $led→hide;
    data→led_set→add($led);
    ... ;
  }
  data→led_set→at(0)→set_pos(120 + 60, 120 - 60);
  data→led_set→at(1)→set_pos(320 + 60, 120 - 60);
  data→led_set→at(2)→set_pos(120 + 60, 320 - 60);
  data→led_set→at(3)→set_pos(320 + 60, 320 - 60);
  data→board→update_on_wall;
  meta private;
}
var led_set : Sprite_Set {
}
var main_tile : Tile {
  readonly = true;
}
var version : Number {
}
table thing {
  type = "Object";
}

