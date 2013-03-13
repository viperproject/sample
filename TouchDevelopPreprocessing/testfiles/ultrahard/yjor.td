meta version "v2.2,nothing";
meta name "MyOS";
meta icon "terminal";
meta color "#ff66ff00";
// MyOS is an operation system for TouchDevelop!
// New in this version (3.1):
// -Instead of tapping Next or Back, you can tap on MyOS to switch between the programs.
// -Added a new boot screen
// -Instead of tapping Menu, you can tap and hold MyOS to open the menu.
// -Bug fixes
meta platform "current";
var setup : Boolean {
}
var user : String {
}
var pw : String {
}

action login() {
  wall→clear;
  $board := media→create_full_board;
  $board→post_to_wall;
  $sprite := $board→create_text(100, 20, 40, "MyOS");
  $sprite1 := $board→create_text(100, 20, 60, "3");
  $x := 0;
  $x1 := 0;
  for 0 ≤ i < 50 do {
    $sprite→set_pos($x, 360);
    $sprite1→set_pos(480 - $x, 345);
    if $x1 = 4 then {
      $sprite→set_color(colors→random);
      $sprite1→set_color(colors→random);
      $x1 := 0;
    }
    else {
      $x1 := $x1 + 1;
    }
    $board→update_on_wall;
    time→sleep(0.05);
    $x := $x + 4;
  }
  $sprite→set_color(data→color);
  $sprite1→set_color(data→color);
  $board→update_on_wall;
  time→sleep(1);
  wall→clear;
  wall→prompt("Welcome to MyOS!");
  if data→logon then {
    skip;
  }
  else {
    code→system;
  }
  wall→clear;
  if not data→setup then {
    wall→prompt("You have to configure MyOS!");
    data→setup := true;
    wall→clear;
    code→configuration;
  }
  wall→clear;
  $x := 0;
  while $x = 0 do {
    wall→clear;
    $s := wall→ask_string("Username:");
    wall→clear;
    $s1 := wall→ask_string("Keyword:");
    wall→clear;
    if data→user→equals($s) then {
      if data→pw→equals($s1) then {
        $x := 1;
        wall→prompt("Login successful.\nHave fun! =)");
        wall→clear;
        code→system;
      }
    }
    else {
      wall→prompt("False entries!");
    }
  }
  meta private;
}

action configuration() {
  wall→clear;
  data→logon := true;
  $s := wall→ask_string("New Username:");
  data→user := $s;
  $x := 0;
  while $x = 0 do {
    wall→clear;
    $s1 := wall→ask_string("New Keyword:");
    wall→clear;
    $s2 := wall→ask_string("Retype Keyword:");
    wall→clear;
    if $s1→equals($s2) then {
      $x := 1;
      data→pw := $s1;
    }
    else {
      wall→prompt("Different Keywords!");
    }
  }
  wall→prompt("Restart!");
  code→login;
  meta private;
}

action system() {
  wall→clear;
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board4 := media→create_board(75);
  $board5 := media→create_board(75);
  $board6 := media→create_board(75);
  $board7 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $board4→set_background(data→color);
  $board5→set_background(data→color);
  $board6→set_background(data→color);
  $board7→set_background(data→color);
  $sprite1 := $board→create_text(100, 20, 40, "MyOS          -->");
  $sprite := $board1→create_text(100, 20, 40, "Phone");
  $sprite2 := $board2→create_text(100, 20, 40, "Camera");
  $sprite3 := $board3→create_text(100, 20, 40, "App Manager");
  $sprite4 := $board4→create_text(100, 20, 40, "Translator");
  $sprite5 := $board5→create_text(100, 20, 40, "Clock");
  $sprite6 := $board6→create_text(100, 20, 40, "Recorder");
  $sprite7 := $board7→create_text(100, 20, 40, "Editor");
  $sprite1→set_pos(230, 20);
  $sprite→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $sprite4→set_pos(100, 20);
  $sprite5→set_pos(100, 20);
  $sprite6→set_pos(100, 20);
  $sprite7→set_pos(100, 20);
  $board7→post_to_wall;
  $board6→post_to_wall;
  $board5→post_to_wall;
  $board4→post_to_wall;
  $board3→post_to_wall;
  $board2→post_to_wall;
  $board1→post_to_wall;
  $board→post_to_wall;
  while true do {
    if $board→touched then {
      time→sleep(0.4);
      if $board→touched then {
        code→menu;
      }
      else {
        code→system2;
      }
    }
    if $board1→touched then {
      code→telephone;
    }
    if $board2→touched then {
      code→camera;
    }
    if $board3→touched then {
      code→apps;
    }
    if $board4→touched then {
      code→translator;
    }
    if $board5→touched then {
      code→clock;
    }
    if $board6→touched then {
      code→recorder;
    }
    if $board7→touched then {
      code→editor;
    }
  }
  meta private;
}
var a : Boolean {
}
var b : Boolean {
}
var c : Boolean {
}
var d : Boolean {
}
var e : Boolean {
}
var bb : Sound {
}
var cc : Sound {
}
var dd : Sound {
}
var ee : Sound {
}

action camera() {
  wall→clear;
  $pic := senses→take_camera_picture;
  $pic→save_to_library;
  $pic→post_to_wall;
  wall→prompt("Photo saved!");
  code→system;
  meta private;
}

action recorder() {
  while true do {
    wall→clear;
    if not data→e then {
      "5: free" →post_to_wall;
    }
    else {
      "5: busy" →post_to_wall;
    }
    if not data→d then {
      "4: free" →post_to_wall;
    }
    else {
      "4: busy" →post_to_wall;
    }
    if not data→c then {
      "3: free" →post_to_wall;
    }
    else {
      "3: busy" →post_to_wall;
    }
    if not data→b then {
      "2: free" →post_to_wall;
    }
    else {
      "2: busy" →post_to_wall;
    }
    if not data→a then {
      "1: free" →post_to_wall;
    }
    else {
      "1: busy" →post_to_wall;
    }
    "" →post_to_wall;
    "4) Exit" →post_to_wall;
    "3) Delete" →post_to_wall;
    "2) Play" →post_to_wall;
    "1) Record" →post_to_wall;
    $x := wall→ask_number("Number");
    if $x→to_string→equals("1") then {
      while true do {
        wall→clear;
        if not data→e then {
          "5) free" →post_to_wall;
        }
        else {
          "5) busy" →post_to_wall;
        }
        if not data→d then {
          "4) free" →post_to_wall;
        }
        else {
          "4) busy" →post_to_wall;
        }
        if not data→c then {
          "3) free" →post_to_wall;
        }
        else {
          "3) busy" →post_to_wall;
        }
        if not data→b then {
          "2) free" →post_to_wall;
        }
        else {
          "2) busy" →post_to_wall;
        }
        if not data→a then {
          "1) free" →post_to_wall;
        }
        else {
          "1) busy" →post_to_wall;
        }
        "" →post_to_wall;
        "0) Back" →post_to_wall;
        "" →post_to_wall;
        "-Existing files were overridden-" →post_to_wall;
        $x := wall→ask_number("Number");
        if $x→to_string→equals("1") then {
          data→a := true;
          data→aa := senses→record_microphone;
        }
        if $x→to_string→equals("2") then {
          data→b := true;
          data→bb := senses→record_microphone;
        }
        if $x→to_string→equals("3") then {
          data→c := true;
          data→cc := senses→record_microphone;
        }
        if $x→to_string→equals("4") then {
          data→d := true;
          data→dd := senses→record_microphone;
        }
        if $x→to_string→equals("5") then {
          data→e := true;
          data→ee := senses→record_microphone;
        }
        if $x→to_string→equals("0") then {
          code→recorder;
        }
      }
    }
    else {
      if $x→to_string→equals("2") then {
        while true do {
          wall→clear;
          if not data→e then {
            "5: free" →post_to_wall;
          }
          else {
            "5) busy" →post_to_wall;
          }
          if not data→d then {
            "4: free" →post_to_wall;
          }
          else {
            "4) busy" →post_to_wall;
          }
          if not data→c then {
            "3: free" →post_to_wall;
          }
          else {
            "3) busy" →post_to_wall;
          }
          if not data→b then {
            "2: free" →post_to_wall;
          }
          else {
            "2) busy" →post_to_wall;
          }
          if not data→a then {
            "1: free" →post_to_wall;
          }
          else {
            "1) busy" →post_to_wall;
          }
          "" →post_to_wall;
          "0) Back" →post_to_wall;
          $x := wall→ask_number("Number");
          if $x→to_string→equals("1") then {
            if data→a then {
              data→aa→post_to_wall;
              wall→prompt("");
            }
          }
          if $x→to_string→equals("2") then {
            if data→b then {
              data→bb→post_to_wall;
              wall→prompt("");
            }
          }
          if $x→to_string→equals("3") then {
            if data→c then {
              data→cc→post_to_wall;
              wall→prompt("");
            }
          }
          if $x→to_string→equals("4") then {
            if data→d then {
              data→dd→post_to_wall;
              wall→prompt("");
            }
          }
          if $x→to_string→equals("5") then {
            if data→e then {
              data→ee→post_to_wall;
              wall→prompt("");
            }
          }
          if $x→to_string→equals("0") then {
            code→recorder;
          }
        }
      }
      else {
        if $x→to_string→equals("3") then {
          while true do {
            wall→clear;
            if not data→e then {
              "5: free" →post_to_wall;
            }
            else {
              "5) busy" →post_to_wall;
            }
            if not data→d then {
              "4: free" →post_to_wall;
            }
            else {
              "4) busy" →post_to_wall;
            }
            if not data→c then {
              "3: free" →post_to_wall;
            }
            else {
              "3) busy" →post_to_wall;
            }
            if not data→b then {
              "2: free" →post_to_wall;
            }
            else {
              "2) busy" →post_to_wall;
            }
            if not data→a then {
              "1: free" →post_to_wall;
            }
            else {
              "1) busy" →post_to_wall;
            }
            "" →post_to_wall;
            "0) Back" →post_to_wall;
            $x := wall→ask_number("Number");
            if $x→to_string→equals("1") then {
              data→a := false;
            }
            if $x→to_string→equals("2") then {
              data→b := false;
            }
            if $x→to_string→equals("3") then {
              data→c := false;
            }
            if $x→to_string→equals("4") then {
              data→d := false;
            }
            if $x→to_string→equals("5") then {
              data→e := false;
            }
            if $x→to_string→equals("0") then {
              code→recorder;
            }
          }
        }
        else {
          if $x→to_string→equals("4") then {
            code→system;
          }
        }
      }
    }
  }
  meta private;
}

action translator() {
  wall→clear;
  if not web→is_connected then {
    wall→prompt("Error: No internet connection available!");
    code→system;
  }
  $s := languages→translate(wall→ask_string("Old language-code:"), wall→ask_string("New language-code:"), wall→ask_string("word:"));
  $s→post_to_wall;
  wall→prompt("Back to menu, click OK!");
  code→system;
  meta private;
}

action menu() {
  wall→clear;
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board4 := media→create_board(75);
  $board5 := media→create_board(75);
  $board6 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $board4→set_background(data→color);
  $board5→set_background(data→color);
  $board6→set_background(data→color);
  $sprite1 := $board→create_text(100, 20, 40, "Menu");
  $sprite := $board1→create_text(100, 20, 40, "Back");
  $sprite2 := $board2→create_text(100, 20, 40, "Coming soon...");
  $sprite3 := $board3→create_text(100, 20, 40, "Lock");
  $sprite4 := $board4→create_text(100, 20, 40, "Restart MyOS");
  $sprite5 := $board5→create_text(100, 20, 40, "Turn off MyOS");
  $sprite6 := $board6→create_text(100, 20, 40, "Settings");
  $sprite1→set_pos(230, 20);
  $sprite→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $sprite4→set_pos(100, 20);
  $sprite5→set_pos(100, 20);
  $sprite6→set_pos(100, 20);
  $board6→post_to_wall;
  $board5→post_to_wall;
  $board4→post_to_wall;
  $board3→post_to_wall;
  $board2→post_to_wall;
  $board1→post_to_wall;
  $board→post_to_wall;
  while true do {
    if $board1→touched then {
      code→system;
    }
    if $board2→touched then {
      skip;
    }
    if $board3→touched then {
      code→locked;
    }
    if $board4→touched then {
      code→Launch;
    }
    if $board5→touched then {
      wall→clear;
      wall→prompt("Goodbye!");
      wall→clear;
      time→stop_and_close;
    }
    if $board6→touched then {
      code→settings;
    }
  }
  meta private;
}

action locked() {
  while true do {
    wall→clear;
    "Enter Password to unlock!" →post_to_wall;
    "----------------------" →post_to_wall;
    "Locked!" →post_to_wall;
    $s := wall→ask_string("Password");
    if data→pw→equals($s) then {
      code→system;
    }
    else {
      wall→prompt("False password!");
    }
  }
  meta private;
}

action reset() {
  wall→clear;
  if wall→ask_boolean("This action delete the configuration of MyOS!", "Are you sure?") then {
    data→setup := false;
    data→bootmgr := false;
    data→color := colors→chrome;
    data→user := "";
    data→pw := "";
    data→ver := "3.1";
    wall→prompt("The configuration of MyOS was deleted!");
    wall→clear;
    time→stop_and_close;
  }
  else {
    code→system;
  }
  meta private;
}

action telephone() {
  wall→clear;
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $sprite := $board→create_text(100, 20, 40, "Phone");
  $sprite1 := $board1→create_text(100, 20, 40, "Telephone");
  $sprite2 := $board2→create_text(100, 20, 40, "SMS");
  $sprite3 := $board3→create_text(100, 20, 40, "Back");
  $sprite→set_pos(220, 20);
  $sprite1→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $board3→post_to_wall;
  $board2→post_to_wall;
  $board1→post_to_wall;
  $board→post_to_wall;
  while not $board3→touched do {
    if $board1→touched then {
      if wall→ask_boolean("Choose a number from contacts?", "Open contacts") then {
        phone→dial_phone_number(phone→choose_phone_number ∥ "");
      }
      else {
        phone→dial_phone_number(wall→ask_string("Enter phone number:"));
      }
      code→telephone;
    }
    if $board2→touched then {
      if wall→ask_boolean("Choose a number from contacts?", "Open contacts") then {
        social→send_sms((phone→choose_phone_number ∥ ""), wall→ask_string("Enter Text"));
      }
      else {
        social→send_sms((wall→ask_number("Enter phone number") ∥ ""), wall→ask_string("Enter Text"));
      }
      code→telephone;
    }
  }
  code→system;
  meta private;
}

action locator() {
  wall→clear;
  if not web→is_connected then {
    wall→prompt("Error: No internet connection is available!");
    code→system;
  }
  $map := maps→create_map;
  $loc := senses→current_location;
  $map→set_center($loc);
  $map→add_text($loc, "My Location", colors→background, colors→foreground);
  $map→post_to_wall;
  wall→prompt("Back to menu, click OK!");
  code→system;
  meta private;
}

action settings() {
  wall→clear;
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board4 := media→create_board(75);
  $board5 := media→create_board(75);
  $board6 := media→create_board(75);
  $board7 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $board4→set_background(data→color);
  $board5→set_background(data→color);
  $board6→set_background(data→color);
  $board7→set_background(data→color);
  $sprite1 := $board→create_text(100, 20, 40, "Settings");
  $sprite := $board1→create_text(100, 20, 40, "Info");
  $sprite2 := $board2→create_text(100, 20, 40, "");
  if data→logon then {
    $sprite2→set_text("Logon ON");
  }
  else {
    $sprite2→set_text("Logon OFF");
  }
  $sprite3 := $board3→create_text(100, 20, 40, "Reset User");
  $sprite4 := $board4→create_text(100, 20, 40, "Reset MyOS");
  $sprite5 := $board5→create_text(100, 20, 40, "Back");
  $sprite6 := $board6→create_text(100, 20, 40, "");
  if data→bootmgr then {
    $sprite6→set_text("Boot Manager ON");
  }
  else {
    $sprite6→set_text("Boot Manager OFF");
  }
  $sprite7 := $board7→create_text(100, 20, 40, "Change color");
  $sprite1→set_pos(200, 20);
  $sprite→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $sprite4→set_pos(100, 20);
  $sprite5→set_pos(100, 20);
  $sprite6→set_pos(100, 20);
  $sprite7→set_pos(100, 20);
  $board5→post_to_wall;
  $board4→post_to_wall;
  $board3→post_to_wall;
  $board7→post_to_wall;
  $board6→post_to_wall;
  $board2→post_to_wall;
  $board1→post_to_wall;
  $board→post_to_wall;
  while true do {
    if $board1→touched then {
      wall→clear;
      "Copyright © 2011-2012 mhSoft\nAll rights reserved." →post_to_wall;
      $s := "MyOS " ∥ data→ver;
      $s→post_to_wall;
      wall→prompt("Info");
      wall→clear;
      code→settings;
    }
    if $board2→touched then {
      if data→logon then {
        data→logon := false;
        code→settings;
      }
      else {
        data→logon := true;
        code→settings;
      }
    }
    if $board3→touched then {
      code→configuration;
    }
    if $board4→touched then {
      code→reset;
    }
    if $board5→touched then {
      code→system;
    }
    if $board6→touched then {
      if data→bootmgr then {
        data→bootmgr := false;
        code→settings;
      }
      else {
        data→bootmgr := true;
        code→settings;
      }
    }
    if $board7→touched then {
      wall→clear;
      $board8 := media→create_board(75);
      $board9 := media→create_board(75);
      $board10 := media→create_board(75);
      $board11 := media→create_board(75);
      $board12 := media→create_board(75);
      $board13 := media→create_board(75);
      $board14 := media→create_board(75);
      $board15 := media→create_board(75);
      $board9→set_background(colors→chrome);
      $board10→set_background(colors→blue);
      $board11→set_background(colors→green→darken(0.2));
      $board12→set_background(colors→red);
      $board13→set_background(colors→orange→darken(0.1));
      $board14→set_background(colors→accent);
      $board15→set_background(data→color);
      $sprite8 := $board8→create_text(100, 20, 40, "Change color");
      $sprite9 := $board9→create_text(100, 20, 40, "Standard");
      $sprite10 := $board14→create_text(100, 20, 40, "Accent");
      $sprite11 := $board15→create_text(100, 20, 40, "Cancel");
      $sprite8→set_pos(150, 20);
      $sprite9→set_pos(100, 20);
      $sprite10→set_pos(100, 20);
      $sprite11→set_pos(100, 20);
      $board15→post_to_wall;
      $board14→post_to_wall;
      $board13→post_to_wall;
      $board12→post_to_wall;
      $board11→post_to_wall;
      $board10→post_to_wall;
      $board9→post_to_wall;
      $board8→post_to_wall;
      while not $board15→touched do {
        if $board9→touched then {
          data→color := colors→chrome;
          code→Launch;
        }
        if $board10→touched then {
          data→color := colors→blue;
          code→Launch;
        }
        if $board11→touched then {
          data→color := colors→green→darken(0.2);
          code→Launch;
        }
        if $board12→touched then {
          data→color := colors→red;
          code→Launch;
        }
        if $board13→touched then {
          data→color := colors→orange→darken(0.1);
          code→Launch;
        }
        if $board14→touched then {
          data→color := colors→accent;
          code→Launch;
        }
      }
      code→system;
    }
  }
  meta private;
}
var logon : Boolean {
}

action connection_center() {
  wall→clear;
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board4 := media→create_board(75);
  $board5 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $board4→set_background(data→color);
  $board5→set_background(data→color);
  $sprite1 := $board→create_text(100, 20, 40, "Connection Center");
  $sprite := $board1→create_text(100, 20, 40, "Airplane-Mode");
  $sprite2 := $board2→create_text(100, 20, 40, "Bluetooth");
  $sprite3 := $board3→create_text(100, 20, 40, "Cellular");
  $sprite4 := $board4→create_text(100, 20, 40, "Wifi");
  $sprite5 := $board5→create_text(100, 20, 40, "Back");
  $sprite1→set_pos(120, 20);
  $sprite→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $sprite4→set_pos(100, 20);
  $sprite5→set_pos(100, 20);
  $board5→post_to_wall;
  $board4→post_to_wall;
  $board3→post_to_wall;
  $board2→post_to_wall;
  $board1→post_to_wall;
  $board→post_to_wall;
  while true do {
    if $board1→touched then {
      web→open_connection_settings("airplanemode");
    }
    if $board2→touched then {
      web→open_connection_settings("bluetooth");
    }
    if $board3→touched then {
      web→open_connection_settings("cellular");
    }
    if $board4→touched then {
      web→open_connection_settings("wifi");
    }
    if $board5→touched then {
      code→system;
    }
  }
  meta private;
}

action clock() {
  wall→clear;
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board4 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $board4→set_background(data→color);
  $sprite1 := $board→create_text(100, 20, 40, "Clock");
  $sprite := $board1→create_text(100, 20, 40, "Back");
  $sprite2 := $board2→create_text(100, 20, 40, "Date & Time");
  $sprite3 := $board3→create_text(100, 20, 40, "Stopwatch");
  $sprite4 := $board4→create_text(100, 20, 40, "Timer");
  $sprite1→set_pos(220, 20);
  $sprite→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $sprite4→set_pos(100, 20);
  $board4→post_to_wall;
  $board3→post_to_wall;
  $board2→post_to_wall;
  $board1→post_to_wall;
  $board→post_to_wall;
  while true do {
    if $board1→touched then {
      code→system;
    }
    if $board2→touched then {
      wall→clear;
      $board5 := media→create_board(75);
      $board6 := media→create_board(75);
      $board7 := media→create_board(75);
      $board6→set_background(data→color);
      $board7→set_background(data→color);
      $sprite5 := $board5→create_text(100, 20, 40, "Date & Time");
      $sprite6 := $board6→create_text(100, 20, 40, time→now ∥ "");
      $sprite7 := $board7→create_text(100, 20, 40, "Back");
      $sprite5→set_pos(170, 20);
      $sprite6→set_pos(100, 20);
      $sprite7→set_pos(100, 20);
      $board7→post_to_wall;
      $board6→post_to_wall;
      $board5→post_to_wall;
      while not $board7→touched do {
        $sprite6→set_text(time→now ∥ "");
        $board6→update_on_wall;
      }
      code→clock;
    }
    if $board3→touched then {
      wall→clear;
      $board8 := media→create_board(75);
      $board9 := media→create_board(75);
      $board10 := media→create_board(75);
      $board11 := media→create_board(75);
      $board12 := media→create_board(75);
      $board9→set_background(data→color);
      $board10→set_background(data→color);
      $board11→set_background(data→color);
      $board12→set_background(data→color);
      $sprite8 := $board8→create_text(100, 20, 40, "Stopwatch");
      $sprite9 := $board9→create_text(100, 20, 40, "0");
      $sprite10 := $board10→create_text(100, 20, 40, "Start");
      $sprite11 := $board11→create_text(100, 20, 40, "Clear");
      $sprite12 := $board12→create_text(100, 20, 40, "Back");
      $sprite8→set_pos(180, 20);
      $sprite9→set_pos(100, 20);
      $sprite10→set_pos(100, 20);
      $sprite11→set_pos(100, 20);
      $sprite12→set_pos(100, 20);
      $board12→post_to_wall;
      $board11→post_to_wall;
      $board10→post_to_wall;
      $board9→post_to_wall;
      $board8→post_to_wall;
      $b := false;
      $x := 0;
      while not $board12→touched do {
        if $board11→touched then {
          $x := 0;
          $sprite9→set_text("0");
          $board9→update_on_wall;
        }
        if $board10→touched then {
          $sprite10→set_text("Stop");
          $board10→update_on_wall;
          $b := true;
        }
        while $b do {
          time→sleep(0.1);
          $x := $x + 0.1;
          $x := $x * 10;
          $x := math→round($x);
          $x := $x / 10;
          $sprite9→set_text($x ∥ "");
          $board9→update_on_wall;
          if $board10→touched then {
            time→sleep(0.1);
            $sprite10→set_text("Start");
            $board10→update_on_wall;
            $b := false;
          }
        }
      }
      code→clock;
    }
    if $board4→touched then {
      wall→clear;
      $board13 := media→create_board(75);
      $board14 := media→create_board(75);
      $board15 := media→create_board(75);
      $board16 := media→create_board(75);
      $board17 := media→create_board(75);
      $board14→set_background(data→color);
      $board15→set_background(data→color);
      $board16→set_background(data→color);
      $board17→set_background(data→color);
      $sprite13 := $board13→create_text(100, 20, 40, "Timer");
      $sprite14 := $board14→create_text(100, 20, 40, "0");
      $sprite15 := $board15→create_text(100, 20, 40, "Start");
      $sprite16 := $board16→create_text(100, 20, 40, "Set");
      $sprite17 := $board17→create_text(100, 20, 40, "Back");
      $sprite13→set_pos(220, 20);
      $sprite14→set_pos(100, 20);
      $sprite15→set_pos(100, 20);
      $sprite16→set_pos(100, 20);
      $sprite17→set_pos(100, 20);
      $board17→post_to_wall;
      $board16→post_to_wall;
      $board15→post_to_wall;
      $board14→post_to_wall;
      $board13→post_to_wall;
      $b1 := false;
      $x1 := 0;
      while not $board17→touched do {
        if $board16→touched then {
          $x1 := wall→ask_number("Minutes:");
          $sprite14→set_text($x1 ∥ "");
          $board14→update_on_wall;
        }
        if $board15→touched then {
          $sprite15→set_text("Stop");
          $board15→update_on_wall;
          $b1 := true;
        }
        while $b1 do {
          time→sleep(6);
          $x1 := $x1 - 0.1;
          $x1 := $x1 * 10;
          $x1 := math→round($x1);
          $x1 := $x1 / 10;
          $sprite14→set_text($x1 ∥ "");
          $board14→update_on_wall;
          if $board15→touched then {
            time→sleep(0.1);
            $sprite15→set_text("Start");
            $board15→update_on_wall;
            $b1 := false;
          }
          if $x1 ≤ 0 then {
            phone→vibrate(3);
            $sprite15→set_text("Start");
            $board15→update_on_wall;
            $b1 := false;
          }
        }
      }
      code→clock;
    }
  }
  meta private;
}
var ver : String {
}

action media_center() {
  wall→clear;
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board4 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $board4→set_background(data→color);
  $sprite := $board→create_text(100, 20, 40, "Media Center");
  $sprite1 := $board1→create_text(100, 20, 40, "Picture Viewer");
  $sprite2 := $board2→create_text(100, 20, 40, "Music");
  $sprite3 := $board3→create_text(100, 20, 40, "Player");
  $sprite4 := $board4→create_text(100, 20, 40, "Back");
  $sprite→set_pos(160, 20);
  $sprite1→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $sprite4→set_pos(100, 20);
  $board4→post_to_wall;
  $board3→post_to_wall;
  $board2→post_to_wall;
  $board1→post_to_wall;
  $board→post_to_wall;
  while not $board4→touched do {
    if $board1→touched then {
      wall→clear;
      $board5 := media→create_board(75);
      $board6 := media→create_board(75);
      $board7 := media→create_board(75);
      $board8 := media→create_board(75);
      $board9 := media→create_board(75);
      $board6→set_background(data→color);
      $board7→set_background(data→color);
      $board8→set_background(data→color);
      $board9→set_background(data→color);
      $sprite5 := $board5→create_text(100, 20, 40, "Picture Viewer");
      $sprite6 := $board6→create_text(100, 20, 40, "");
      $sprite6→set_text(media→picture_albums→count ∥ " Album(s)");
      $sprite7 := $board7→create_text(100, 20, 40, "");
      $sprite7→set_text(media→pictures→count ∥ " Picture(s)");
      $sprite8 := $board8→create_text(100, 20, 40, "Select");
      $sprite9 := $board9→create_text(100, 20, 40, "Back");
      $sprite5→set_pos(160, 20);
      $sprite6→set_pos(100, 20);
      $sprite7→set_pos(100, 20);
      $sprite8→set_pos(100, 20);
      $sprite9→set_pos(100, 20);
      $board9→post_to_wall;
      $board8→post_to_wall;
      $board7→post_to_wall;
      $board6→post_to_wall;
      $board5→post_to_wall;
      while not $board9→touched do {
        if $board8→touched then {
          wall→clear;
          $pic := media→choose_picture;
          $pic→post_to_wall;
          wall→prompt("Selected Picture:");
          code→media_center;
        }
      }
      code→media_center;
    }
    if $board2→touched then {
      wall→clear;
      $board10 := media→create_board(75);
      $board11 := media→create_board(75);
      $board12 := media→create_board(75);
      $board13 := media→create_board(75);
      $board14 := media→create_board(75);
      $board11→set_background(data→color);
      $board12→set_background(data→color);
      $board13→set_background(data→color);
      $board14→set_background(data→color);
      $sprite10 := $board10→create_text(100, 20, 40, "Music");
      $sprite11 := $board11→create_text(100, 20, 40, "");
      $sprite11→set_text(media→song_albums→count ∥ " Album(s)");
      $sprite12 := $board12→create_text(100, 20, 40, "");
      $sprite12→set_text(media→songs→count ∥ " Song(s)");
      $sprite13 := $board13→create_text(100, 20, 40, "Select");
      $sprite14 := $board14→create_text(100, 20, 40, "Back");
      $sprite10→set_pos(210, 20);
      $sprite11→set_pos(100, 20);
      $sprite12→set_pos(100, 20);
      $sprite13→set_pos(100, 20);
      $sprite14→set_pos(100, 20);
      $board14→post_to_wall;
      $board13→post_to_wall;
      $board12→post_to_wall;
      $board11→post_to_wall;
      $board10→post_to_wall;
      while not $board14→touched do {
        if $board13→touched then {
          wall→clear;
          $s := wall→ask_string("Search (for songs!):");
          wall→clear;
          foreach song in media→songs where $song→name→contains($s) do {
            $song→post_to_wall;
          }
          wall→prompt("Found:");
          code→media_center;
        }
      }
      code→media_center;
    }
    if $board3→touched then {
      wall→clear;
      $board15 := media→create_board(75);
      $board16 := media→create_board(75);
      $board17 := media→create_board(75);
      $board18 := media→create_board(75);
      $board19 := media→create_board(75);
      $board20 := media→create_board(75);
      $board21 := media→create_board(75);
      $board16→set_background(data→color);
      $board17→set_background(data→color);
      $board18→set_background(data→color);
      $board19→set_background(data→color);
      $board20→set_background(data→color);
      $board21→set_background(data→color);
      $sprite17 := $board15→create_text(100, 20, 40, "Player");
      $sprite18 := $board16→create_text(100, 20, 40, "");
      $sprite19 := $board17→create_text(100, 20, 40, "Play/Pause");
      $sprite20 := $board18→create_text(100, 20, 40, "< Previous");
      $sprite21 := $board19→create_text(100, 20, 40, "Next >");
      $sprite22 := $board20→create_text(100, 20, 40, "Stop");
      $sprite23 := $board21→create_text(100, 20, 40, "Back");
      $sprite17→set_pos(210, 20);
      $sprite18→set_pos(100, 20);
      $sprite19→set_pos(100, 20);
      $sprite20→set_pos(100, 20);
      $sprite21→set_pos(100, 20);
      $sprite22→set_pos(100, 20);
      $sprite23→set_pos(100, 20);
      $board21→post_to_wall;
      $board20→post_to_wall;
      $board19→post_to_wall;
      $board18→post_to_wall;
      $board17→post_to_wall;
      $board16→post_to_wall;
      $board15→post_to_wall;
      $b := false;
      while not $board21→touched do {
        if player→is_playing then {
          $sprite18→set_text(player→active_song→name);
        }
        else {
          $sprite18→set_text("Now playing: -");
        }
        if $board17→touched then {
          if player→is_playing then {
            $b := true;
            player→pause;
          }
          else {
            if $b then {
              player→resume;
            }
            else {
              media→songs→play;
            }
          }
          time→sleep(0.1);
        }
        if $board18→touched then {
          time→sleep(0.1);
          player→previous;
        }
        if $board19→touched then {
          time→sleep(0.1);
          player→next;
        }
        if $board20→touched then {
          $b := false;
          player→stop;
        }
      }
      code→media_center;
    }
  }
  code→system;
  meta private;
}

action system2() {
  wall→clear;
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board4 := media→create_board(75);
  $board5 := media→create_board(75);
  $board6 := media→create_board(75);
  $board7 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $board4→set_background(data→color);
  $board5→set_background(data→color);
  $board6→set_background(data→color);
  $board7→set_background(data→color);
  $sprite1 := $board→create_text(100, 20, 40, "<--           MyOS");
  $sprite := $board1→create_text(100, 20, 40, "");
  $sprite2 := $board2→create_text(100, 20, 40, "Media Center");
  $sprite3 := $board3→create_text(100, 20, 40, "Connection Center");
  $sprite4 := $board4→create_text(100, 20, 40, "");
  $sprite5 := $board5→create_text(100, 20, 40, "");
  $sprite6 := $board6→create_text(100, 20, 40, "Locate me");
  $sprite7 := $board7→create_text(100, 20, 40, "");
  $sprite1→set_pos(50, 20);
  $sprite→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $sprite4→set_pos(100, 20);
  $sprite5→set_pos(100, 20);
  $sprite6→set_pos(100, 20);
  $sprite7→set_pos(100, 20);
  $board6→post_to_wall;
  $board3→post_to_wall;
  $board2→post_to_wall;
  $board→post_to_wall;
  while true do {
    if $board→touched then {
      time→sleep(0.4);
      if $board→touched then {
        code→menu;
      }
      else {
        code→system;
      }
    }
    if $board1→touched then {
      skip;
    }
    if $board2→touched then {
      code→media_center;
    }
    if $board3→touched then {
      code→connection_center;
    }
    if $board4→touched then {
      skip;
    }
    if $board5→touched then {
      skip;
    }
    if $board6→touched then {
      code→locator;
    }
    if $board7→touched then {
      skip;
    }
  }
  meta private;
}
var aa : Sound {
}

action editor() {
  wall→clear;
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board4 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $board4→set_background(data→color);
  $sprite1 := $board→create_text(100, 20, 40, "Editor");
  $sprite := $board1→create_text(100, 20, 40, "Back");
  $sprite2 := $board2→create_text(100, 20, 40, "New");
  $sprite3 := $board3→create_text(100, 20, 40, "Open");
  $sprite4 := $board4→create_text(100, 20, 40, "Delete");
  $sprite1→set_pos(220, 20);
  $sprite→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $sprite4→set_pos(100, 20);
  $board4→post_to_wall;
  $board3→post_to_wall;
  $board2→post_to_wall;
  $board1→post_to_wall;
  $board→post_to_wall;
  while not $board1→touched do {
    if $board2→touched then {
      wall→clear;
      $board := media→create_board(75);
      $board1 := media→create_board(75);
      $board2 := media→create_board(75);
      $board3 := media→create_board(75);
      $board4 := media→create_board(75);
      $board1→set_background(data→color);
      $board2→set_background(colors→red);
      $board3→set_background(colors→red);
      $board4→set_background(colors→red);
      if data→t1→is_empty then {
        $board2→set_background(colors→green);
      }
      if data→t2→is_empty then {
        $board3→set_background(colors→green);
      }
      if data→t3→is_empty then {
        $board4→set_background(colors→green);
      }
      $sprite1 := $board→create_text(100, 20, 40, "New");
      $sprite := $board1→create_text(100, 20, 40, "Back");
      $sprite2 := $board2→create_text(100, 20, 40, "1. Save");
      $sprite3 := $board3→create_text(100, 20, 40, "2. Save");
      $sprite4 := $board4→create_text(100, 20, 40, "3. Save");
      $sprite1→set_pos(220, 20);
      $sprite→set_pos(100, 20);
      $sprite2→set_pos(100, 20);
      $sprite3→set_pos(100, 20);
      $sprite4→set_pos(100, 20);
      $sprite2→set_color(colors→black);
      $sprite3→set_color(colors→black);
      $sprite4→set_color(colors→black);
      $board4→post_to_wall;
      $board3→post_to_wall;
      $board2→post_to_wall;
      $board1→post_to_wall;
      $board→post_to_wall;
      while not $board1→touched do {
        if $board2→touched then {
          if data→t1→is_empty then {
            wall→clear;
            data→t1 := wall→ask_string("Write Text:");
            code→editor;
          }
        }
        if $board3→touched then {
          if data→t2→is_empty then {
            wall→clear;
            data→t2 := wall→ask_string("Write Text:");
            code→editor;
          }
        }
        if $board4→touched then {
          if data→t3→is_empty then {
            wall→clear;
            data→t3 := wall→ask_string("Write Text:");
            code→editor;
          }
        }
      }
      code→editor;
    }
    if $board3→touched then {
      wall→clear;
      $board := media→create_board(75);
      $board1 := media→create_board(75);
      $board2 := media→create_board(75);
      $board3 := media→create_board(75);
      $board4 := media→create_board(75);
      $board1→set_background(data→color);
      $board2→set_background(colors→green);
      $board3→set_background(colors→green);
      $board4→set_background(colors→green);
      if data→t1→is_empty then {
        $board2→set_background(colors→red);
      }
      if data→t2→is_empty then {
        $board3→set_background(colors→red);
      }
      if data→t3→is_empty then {
        $board4→set_background(colors→red);
      }
      $sprite1 := $board→create_text(100, 20, 40, "Open");
      $sprite := $board1→create_text(100, 20, 40, "Back");
      $sprite2 := $board2→create_text(100, 20, 40, "1. Save");
      $sprite3 := $board3→create_text(100, 20, 40, "2. Save");
      $sprite4 := $board4→create_text(100, 20, 40, "3. Save");
      $sprite1→set_pos(220, 20);
      $sprite→set_pos(100, 20);
      $sprite2→set_pos(100, 20);
      $sprite3→set_pos(100, 20);
      $sprite4→set_pos(100, 20);
      $sprite2→set_color(colors→black);
      $sprite3→set_color(colors→black);
      $sprite4→set_color(colors→black);
      $board4→post_to_wall;
      $board3→post_to_wall;
      $board2→post_to_wall;
      $board1→post_to_wall;
      $board→post_to_wall;
      while not $board1→touched do {
        if $board2→touched then {
          if not data→t1→is_empty then {
            wall→clear;
            wall→prompt(data→t1);
            code→editor;
          }
        }
        if $board3→touched then {
          if not data→t2→is_empty then {
            wall→clear;
            wall→prompt(data→t2);
            code→editor;
          }
        }
        if $board4→touched then {
          if not data→t3→is_empty then {
            wall→clear;
            wall→prompt(data→t3);
            code→editor;
          }
        }
      }
      code→editor;
    }
    if $board4→touched then {
      wall→clear;
      $board := media→create_board(75);
      $board1 := media→create_board(75);
      $board2 := media→create_board(75);
      $board3 := media→create_board(75);
      $board4 := media→create_board(75);
      $board1→set_background(data→color);
      $board2→set_background(colors→green);
      $board3→set_background(colors→green);
      $board4→set_background(colors→green);
      if data→t1→is_empty then {
        $board2→set_background(colors→red);
      }
      if data→t2→is_empty then {
        $board3→set_background(colors→red);
      }
      if data→t3→is_empty then {
        $board4→set_background(colors→red);
      }
      $sprite1 := $board→create_text(100, 20, 40, "Delete");
      $sprite := $board1→create_text(100, 20, 40, "Back");
      $sprite2 := $board2→create_text(100, 20, 40, "1. Save");
      $sprite3 := $board3→create_text(100, 20, 40, "2. Save");
      $sprite4 := $board4→create_text(100, 20, 40, "3. Save");
      $sprite1→set_pos(220, 20);
      $sprite→set_pos(100, 20);
      $sprite2→set_pos(100, 20);
      $sprite3→set_pos(100, 20);
      $sprite4→set_pos(100, 20);
      $sprite2→set_color(colors→black);
      $sprite3→set_color(colors→black);
      $sprite4→set_color(colors→black);
      $board4→post_to_wall;
      $board3→post_to_wall;
      $board2→post_to_wall;
      $board1→post_to_wall;
      $board→post_to_wall;
      while not $board1→touched do {
        if $board2→touched then {
          if not data→t1→is_empty then {
            wall→clear;
            data→t1 := "";
            wall→prompt("Text deleted!");
            code→editor;
          }
        }
        if $board3→touched then {
          if not data→t2→is_empty then {
            wall→clear;
            data→t2 := "";
            wall→prompt("Text deleted!");
            code→editor;
          }
        }
        if $board4→touched then {
          if not data→t3→is_empty then {
            wall→clear;
            data→t3 := "";
            wall→prompt("Text deleted!");
            code→editor;
          }
        }
      }
      code→editor;
    }
  }
  code→system;
  meta private;
}
var t1 : String {
}
var t2 : String {
}
var t3 : String {
}

action Launch() {
  wall→clear;
  if not data→setup then {
    code→login;
  }
  if not data→bootmgr then {
    code→login;
  }
  $board := media→create_board(75);
  $board1 := media→create_board(75);
  $board2 := media→create_board(75);
  $board3 := media→create_board(75);
  $board4 := media→create_board(75);
  $board5 := media→create_board(75);
  $board1→set_background(data→color);
  $board2→set_background(data→color);
  $board3→set_background(data→color);
  $board4→set_background(data→color);
  $board5→set_background(data→color);
  $sprite := $board→create_text(100, 20, 40, "Boot Manager");
  // Add here the name of the OS
  $sprite1 := $board1→create_text(100, 20, 40, "MyOS");
  $sprite2 := $board2→create_text(100, 20, 40, "");
  $sprite3 := $board3→create_text(100, 20, 40, "");
  $sprite4 := $board4→create_text(100, 20, 40, "");
  $sprite5 := $board5→create_text(100, 20, 40, "");
  $sprite→set_pos(150, 20);
  $sprite1→set_pos(100, 20);
  $sprite2→set_pos(100, 20);
  $sprite3→set_pos(100, 20);
  $sprite4→set_pos(100, 20);
  $sprite5→set_pos(100, 20);
  // Add here the board with the OS.
  $board1→post_to_wall;
  $board→post_to_wall;
  while true do {
    // Add here the launch for the OS and don't forget to add the library!
    if $board1→touched then {
      wall→clear;
      code→login;
    }
    if $board2→touched then {
      wall→clear;
    }
    if $board3→touched then {
      wall→clear;
    }
    if $board4→touched then {
      wall→clear;
    }
    if $board5→touched then {
      wall→clear;
    }
  }
}
var bootmgr : Boolean {
}

action apps() {
  while true do {
    wall→clear;
    "App Manager for MyOS\n-----------------------------------\nType \'show\' to show all apps!\nType \'exit\' to exit app manager!" →post_to_wall;
    $s := wall→ask_string("Enter an app name:");
    $s := $s→to_lower_case;
    if $s→equals("show") then {
      wall→clear;
      // Add here the command of the app in lower case!
      "show -> Show all apps" →post_to_wall;
      "exit -> Exit app manager" →post_to_wall;
      wall→prompt("");
      code→apps;
    }
    if $s→equals("exit") then {
      code→system;
    }
    // Add here the app command in lower case and the Launch-script if the library.
    if $s→equals("") then {
      skip;
    }
  }
  meta private;
}
var color : Color {
}

action go() {
  skip;
  meta private;
}
