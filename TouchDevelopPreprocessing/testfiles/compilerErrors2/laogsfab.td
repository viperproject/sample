meta version "v2.2,nothing";
meta name "Secure Messaging";
meta icon "im";
meta color "#ffa52a2a";
//
meta platform "current";
meta import One\u002DTime_Encryption {
  pub "ozdlhigg"
  usage {
    action Encrypt(clearText: String, showWorking: Boolean) returns cipherText: String
    action Decrypt(cipherText: String, showWorking: Boolean) returns clearText: String
    action Generate_Key(bitsRequired: Number, showWorking: Boolean) returns key: String
  }
  resolve Converter = ♻Converter with {
  }
}
meta import Typewriter {
  pub "xcrx"
  usage {
    action Typewriter(initialText: String, finalText: String, charsPerSecond: Number)
  }
}

action write() {
  // Write a message then send it to a chosen recipient.
  $tiles := collections→create_string_collection;
  code→createWallPage("Write message", data→appSettings→at("appName"), $tiles);
  $clearText := wall→ask_string("Type your message, then press go.");
  wall→clear;
  //
  $SMSrequired := $clearText→count * 8 / data→appSettings→at("SMSsize")→to_number;
  if $SMSrequired ≥ data→appSettings→at("SMSlimit")→to_number then {
    wall→prompt("Your message will take " ∥ $SMSrequired ∥ " SMS messages to send, you may want to use an email.");
  }
  //
  $cipherText := ♻ One\u002DTime_Encryption→Encrypt($clearText, false);
  $cipherText→share("");
  wall→clear;
  //
  if wall→ask_boolean("Would you like to save this message? (It will be saved as clear text)", "Save message?")→equals(true) then {
    code→save("Sent", $clearText);
  }
  else {
    //
  }
  code→removeWallPage;
  meta private;
}

action read() {
  // Type or paste encrypted data, choose the correct decryption key and the original message will be displayed.
  $tiles := collections→create_string_collection;
  code→createWallPage("Read message", data→appSettings→at("appName"), $tiles);
  $cipherText := wall→ask_string("Paste the message here, then press go.");
  wall→clear;
  //
  $clearText := ♻ One\u002DTime_Encryption→Decrypt($cipherText, false);
  ♻ Typewriter→Typewriter("The message is:\n", "The message is:\n" ∥ $clearText, 7);
  if wall→ask_boolean("Would you like to save this message? (It will be saved as clear text)", "Save message?")→equals(true) then {
    code→save("Inbox", $clearText);
  }
  //
  code→removeWallPage;
  meta private;
}

action generateKey() {
  // Pre-generate an encryption key to share with someone.
  $tiles := collections→create_string_collection;
  code→createWallPage("Generate key", data→appSettings→at("appName"), $tiles);
  //
  $keySize := wall→ask_number("What size key would you like to generate?");
  $key := ♻ One\u002DTime_Encryption→Generate_Key($keySize, false);
  wall→clear;
  //
  ♻ Typewriter→Typewriter("The generated encryption key is:\n", "The generated encryption key is:\n" ∥ $key, math→max(7, $keySize / 8));
  $key→copy_to_clipboard;
  //
  "The key has been copied to the clipboard." →post_to_wall;
  wall→prompt("Press OK to return to the menu.");
  code→removeWallPage;
  meta private;
}

action Secure_Messaging_App() {
  code→runtimeSetup;
  $tiles := ("Write message\nRead message\nSaved messages\nGenerate key\nExit " ∥ data→appSettings→at("appName"))→split("\n");
  code→createWallPage("Home", data→appSettings→at("appName"), $tiles);
}
var Inbox : String_Map {
  // Archived received message / key pairs.
}
var Sent : String_Map {
  // Archived sent messages / key pairs.
}

action firstRun() {
  // First-run setup.
  // Put setting names in this string, one per line.
  //
  data→appSettings := collections→create_string_map;
  //
  $settings := "appName\nappVersion\nSMSsize\nSMSlimit";
  data→appSettings→keys→add_many($settings→split("\n"));
  data→appSettings→set_at("appName", "Secure Messaging App");
  data→appSettings→set_at("appVersion", "0.1");
  data→appSettings→set_at("SMSsize", "160");
  data→appSettings→set_at("SMSlimit", "3");
  //
  data→Inbox := collections→create_string_map;
  //
  data→Sent := collections→create_string_map;
  meta private;
}

action createWallPage(title: String, subtitle: String, tiles: String_Collection) {
  // Add a new wall page, set the title and update the breadcrumb.
  wall→push_new_page;
  wall→set_reversed(true);
  wall→set_title($title);
  wall→set_subtitle($subtitle);
  wall→set_background(colors→background);
  wall→set_foreground(colors→accent);
  //
  foreach tileText in $tiles where true do {
    $tile := wall→create_text_box("\n" ∥ $tileText ∥ "\n", 36);
    $tile→post_to_wall;
    $tile→set_background(colors→accent);
    $tile→set_foreground(data→tileTextColour);
  }
  meta private;
}

event tap_wall_TextBox(tile: TextBox) {
  // UI branching.
  data→appBreadcrumb→add($tile→text→trim("\n"));
  $breadcrumb := data→appBreadcrumb→at(data→appBreadcrumb→count - 1);
  if data→appBreadcrumb→at(0)→equals("Home") then {
    if data→appBreadcrumb→at(1)→equals("Exit " ∥ data→appSettings→at("appName")) then {
      time→stop_and_close;
    }
    else {
      if data→appBreadcrumb→at(1)→equals("Write message") then {
        code→write;
        data→appBreadcrumb→remove_at(data→appBreadcrumb→count);
      }
      else {
        if data→appBreadcrumb→at(1)→equals("Read message") then {
          code→read;
        }
        else {
          if data→appBreadcrumb→at(1)→equals("Saved messages") then {
            if data→appBreadcrumb→count = 2 then {
              code→savedChooseFolder;
            }
            else {
              if data→appBreadcrumb→count = 3 then {
                code→savedBrowseFolder($breadcrumb);
              }
            }
          }
        }
        if data→appBreadcrumb→at(1)→equals("Generate key") then {
          code→generateKey;
        }
      }
    }
  }
  meta private;
}
var appSettings : String_Map {
  // App settings.
}

action save(store: String, message: String) {
  // Save the given message in the inbox or sent items, with or without a key.
  // Optionally save the contact's name too.
  $tiles := collections→create_string_collection;
  code→createWallPage("Save", data→appSettings→at("appName"), $tiles);
  $contact := "Unknown";
  if wall→ask_boolean("Would you like to save the contact\'s name with this message?", "Save contact name?")→equals(true) then {
    $contact := social→choose_contact→name;
  }
  $index := time→now→to_string ∥ "\n" ∥ $contact ∥ $message;
  //
  if $store→equals("Inbox") then {
    data→Inbox→keys→add($index);
  }
  else {
    data→Sent→keys→add($index);
  }
  //
  wall→prompt("Your message has been saved. Press OK to return to the menu.");
  code→removeWallPage;
  meta private;
}

action savedChooseFolder() {
  // Choose between Inbox and Sent messages.
  $tiles := "Inbox\nSent" →split("\n");
  code→createWallPage("Choose folder", data→appSettings→at("appName"), $tiles);
  meta private;
}

action createWallPage2(title: String, subtitle: String, tiles: String_Collection) {
  wall→set_reversed(true);
  wall→set_title($title);
  wall→set_subtitle($subtitle);
  $homeBoard := media→create_board(640);
  for 0 ≤ i < 4 do {
    $col := math→mod($i, 2);
    $x := (400 * $col) + (15 * $col);
    $row := math→floor($i / 2);
    $y := (400 * $row) + (15 * $row);
    $tile := $homeBoard→create_rectangle(400, 400);
    $tile→set_color(colors→accent);
    $tile→set_pos($x, $y);
  }
  $homeBoard→post_to_wall;
  meta private;
}

action savedBrowseFolder(store: String) {
  // Browse and read saved messages.
  $messages := collections→create_string_collection;
  if $store→equals("Inbox") then {
    $messages := data→Inbox→keys;
  }
  else {
    $messages := data→Sent→keys;
  }
  code→createWallPage($store, data→appSettings→at("appName"), $messages);
  meta private;
}

action runtimeSetup() {
  // Runtime setup.
  data→appBreadcrumb := collections→create_string_collection;
  data→appBreadcrumb→add("Home");
  if data→appSettings→is_invalid then {
    code→firstRun;
  }
  //
  if math→round((colors→accent→R + colors→accent→G + colors→accent→B)) / 3 = 1 then {
    data→tileTextColour := colors→black;
  }
  else {
    data→tileTextColour := colors→white;
  }
  //

  meta private;
}
var tileTextColour : Color {
  // Colour of text on tiles
}
var appBreadcrumb : String_Collection {
  // Tracks location within the app.
}

action removeWallPage() {
  // Updates the breadcrumb and returns to the previous wall page.
  wall→pop_page;
  meta private;
}

event page_navigated_from() {
  data→appBreadcrumb→remove_at(data→appBreadcrumb→count - 1);
  meta private;
}

