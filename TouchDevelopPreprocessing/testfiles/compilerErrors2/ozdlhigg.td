meta version "v2.2,nothing";
meta name "One-Time Encryption";
meta icon "lock";
meta color "#ff007fff";
// An implementation of Shannon's cipher from 1949.
meta isLibrary "yes";
meta platform "current";
meta import Converter {
  pub "oajw"
  usage {
    action binary\u002DUnicode(binary: Number_Collection, debug: Boolean) returns unicode: Number_Collection
    action binary\u002Dtext(binary: Number_Collection, debug: Boolean) returns text: String
    action text\u002Dbinary(text: String, debug: Boolean) returns binary: Number_Collection
    action text\u002DUnicode(text: String, debug: Boolean) returns unicode: Number_Collection
    action Unicode\u002Dbinary(unicode: Number_Collection, debug: Boolean) returns binary: Number_Collection
    action Unicode\u002Dtext(unicode: Number_Collection, debug: Boolean) returns text: String
  }
}

action pictureKey(bitsRequired: Number, showWorking: Boolean) returns keyBinary: Number_Collection {
  $keyBinary := collections→create_number_collection;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Picture Key");
    wall→set_reversed(true);
    ("Generating picture key of " ∥ $bitsRequired ∥ " bits")→post_to_wall;
  }
  $keyPicture := media→choose_picture;
  $width := $keyPicture→width;
  $height := $keyPicture→height;
  $pixels := $width * $height;
  while $bitsRequired > $pixels do {
    wall→prompt("The picture you gave chosen is too small to encrypt this message. Please choose another.");
    $keyPicture := media→choose_picture;
    $width := $keyPicture→width;
    $height := $keyPicture→height;
    $pixels := $width * $height;
  }
  if $showWorking→equals(true) then {
    "Key picture is:" →post_to_wall;
    $keyPicture→post_to_wall;
    ("Dimensions: " ∥ $width ∥ "x" ∥ $height)→post_to_wall;
    ("Total pixels: " ∥ $pixels)→post_to_wall;
  }
  $steps := math→floor($pixels / $bitsRequired / 2 / 2);
  for 0 ≤ i < $bitsRequired do {
    // Start at the centre pixel.
    $x := math→floor($width / 2);
    $y := math→floor($height / 2);
    // Calculate the offset.
    $x1 := math→mod($i * $steps + 1, math→floor($width / 2));
    $y1 := math→floor(($i * $steps + 1) / math→floor($width / 2));
    // Invert the direction for every other pixel.
    if math→mod($i, 2) = 0 then {
      $x := $x + $x1;
      $y := $y + $y1;
    }
    else {
      $x := $x - $x1;
      $y := $y - $y1;
    }
    $colour := $keyPicture→pixel($x, $y);
    $bit := math→round(($colour→R + $colour→G + $colour→B) / 3);
    if $showWorking→equals(true) then {
      ("Pixel " ∥ $i ∥ " (" ∥ $x ∥ "," ∥ $y ∥ ")")→post_to_wall;
      $colour→post_to_wall;
      ("Average of R,G,B is " ∥ $bit)→post_to_wall;
    }
    $keyBinary→add($bit);
  }
  if $showWorking→equals(true) then {
    "Key data is:" →post_to_wall;
    $keyBinary→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

action XOR(input1: Number_Collection, input2: Number_Collection, showWorking: Boolean) returns output: Number_Collection {
  // Returns an array of binary digits formed by combing two arrays with an XOR operation.
  time→log(time→now ∥ " - Starting XOR operation.");
  $output := collections→create_number_collection;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("XOR");
    wall→set_reversed(true);
    "XOR input 1:" →post_to_wall;
    $input1→post_to_wall;
    "XOR input 2:" →post_to_wall;
    $input2→post_to_wall;
  }
  for 0 ≤ i < $input1→count do {
    if $input1→at($i) = 0 and $input2→at($i) = 0 then {
      $output→add(0);
    }
    else {
      if $input1→at($i) = 1 and $input2→at($i) = 1 then {
        $output→add(0);
      }
      else {
        if $input1→at($i) = 1 or $input2→at($i) = 1 then {
          $output→add(1);
        }
      }
    }
  }
  if $showWorking→equals(true) then {
    "XOR output:" →post_to_wall;
    $output→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  time→log(time→now ∥ " - XOR operation complete.");
  meta private;
}

action One\u002DTime_Encryption_Demo_() {
  // Demonstrates the encryption and decryption process.
  time→log(time→now ∥ " - Starting encryption/decryption demonstration.");
  wall→set_reversed(true);
  wall→set_title("Encryption Demo");
  $testMessage := "";
  $testMessage := wall→ask_string("Enter your test message and press go.");
  while $testMessage→equals("") do {
    $testMessage := wall→ask_string("The text input failed, please re-enter your test message and press go.");
  }
  wall→create_text_box("Original message:\n" ∥ $testMessage, 36)→post_to_wall;
  $testMessage := code→Decrypt(code→Encrypt($testMessage, true), true);
  wall→create_text_box("Decrypted message:\n" ∥ $testMessage, 36)→post_to_wall;
  time→log(time→now ∥ " - Encryption/decryption demonstration complete.");
}

action Encrypt(clearText: String, showWorking: Boolean) returns cipherText: String {
  // Returns the clear text input as an encrypted binary string.
  time→log(time→now ∥ " - Starting encryption operation.");
  $cipherText := "";
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Encrypt");
    wall→set_reversed(true);
    "Clear text:" →post_to_wall;
    $clearText→post_to_wall;
  }
  // Convert the cleartext into unicode data.
  $unicode := ♻ Converter→text\u002DUnicode($clearText, $showWorking);
  if $showWorking→equals(true) then {
    "Unicode data:" →post_to_wall;
    $unicode→post_to_wall;
  }
  // Convert the Unicode values to binary.
  $clearBinary := ♻ Converter→Unicode\u002Dbinary($unicode, $showWorking);
  if $showWorking→equals(true) then {
    "Clear binary:" →post_to_wall;
    $clearBinary→post_to_wall;
  }
  // Generate the encryption key.
  $keyBinary := code→generateKey($clearBinary→count, $showWorking);
  if $showWorking→equals(true) then {
    "Key binary:" →post_to_wall;
    $keyBinary→post_to_wall;
  }
  // For the user's convenience, copy the encryption key to the clipboard.
  ♻ Converter→binary\u002Dtext($keyBinary, $showWorking)→copy_to_clipboard;
  wall→prompt("The key data has been copied to the clipboard, make sure the recipient has a copy before sending your message.\n\nPress OK when you are ready to proceed.");
  // Combine the binary data with the encryption key.
  $cipherBinary := code→XOR($clearBinary, $keyBinary, $showWorking);
  if $showWorking→equals(true) then {
    "Cipher binary:" →post_to_wall;
    $cipherBinary→post_to_wall;
  }
  // Convert the binary data into a string.
  $cipherText := ♻ Converter→binary\u002Dtext($cipherBinary, $showWorking);
  if $showWorking→equals(true) then {
    "Cipher text:" →post_to_wall;
    $cipherText→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  time→log(time→now ∥ " - Encryption operation complete.");
}

action Decrypt(cipherText: String, showWorking: Boolean) returns clearText: String {
  // Returns the encrypted binary input as clear text.
  $clearText := "";
  time→log(time→now ∥ " - Starting decryption operation.");
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Decrypt");
    wall→set_reversed(true);
    "Cipher text:" →post_to_wall;
    $cipherText→post_to_wall;
  }
  // Generate the decryption key.
  $keyBinary := code→generateKey($cipherText→count, $showWorking);
  if $showWorking→equals(true) then {
    "Key binary:" →post_to_wall;
    $keyBinary→post_to_wall;
  }
  // Split the binary string into a collection of binary digits.
  $cipherBinary := ♻ Converter→text\u002Dbinary($cipherText, $showWorking);
  if $showWorking→equals(true) then {
    "Cipher binary:" →post_to_wall;
    $cipherBinary→post_to_wall;
  }
  // Combine the binary data with the decryption key.
  $clearBinary := code→XOR($cipherBinary, $keyBinary, $showWorking);
  if $showWorking→equals(true) then {
    "Clear binary:" →post_to_wall;
    $clearBinary→post_to_wall;
  }
  // Convert the binary data into Unicode.
  $clearUnicode := ♻ Converter→binary\u002DUnicode($clearBinary, $showWorking);
  if $showWorking→equals(true) then {
    "Unicode data:" →post_to_wall;
    $clearUnicode→post_to_wall;
  }
  // Read the text from the Unicode binary data.
  $clearText := ♻ Converter→Unicode\u002Dtext($clearUnicode, $showWorking);
  if $showWorking→equals(true) then {
    "Clear text:" →post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  time→log(time→now ∥ " - Decryption operation complete.");
}

action generateKey(bitsRequired: Number, showWorking: Boolean) returns keyBinary: Number_Collection {
  $keyBinary := collections→create_number_collection;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Generate key");
    wall→set_reversed(true);
    ("Generating a key of " ∥ $bitsRequired ∥ " bits")→post_to_wall;
  }
  $keyTypes := collections→create_string_collection;
  $keyTypes→add("Picture");
  $keyTypes→add("Generate random data");
  $keyTypes→add("Pre-generated random data");
  $keyTypeChoice := 0;
  $keyPrompt := "What would you like to use for the encryption key?\n\nThe sender and receiver must use identical keys. Using random data is more secure, but using a picture will make it easier to share the key.";
  $keyTypeChoice := wall→pick_string("What would you like to use for the key data?", "Choose key type", $keyTypes);
  if $keyTypeChoice = $keyTypes→index_of("Picture", 0) then {
    $keyBinary := code→pictureKey($bitsRequired, $showWorking);
  }
  if $keyTypeChoice = $keyTypes→index_of("Generate random data", 0) then {
    $keyBinary := code→randomKey($bitsRequired, $showWorking);
  }
  if $keyTypeChoice = $keyTypes→index_of("Pre-generated random data", 0) then {
    $keyBinary := code→pre\u002DgenKey($bitsRequired, $showWorking);
  }
  if $showWorking→equals(true) then {
    "Key data:" →post_to_wall;
    $keyBinary→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

action randomKey(bitsRequired: Number, showWorking: Boolean) returns keyBinary: Number_Collection {
  $keyBinary := collections→create_number_collection;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Random key");
    wall→set_reversed(true);
    ("Generating random key of " ∥ $bitsRequired ∥ " bits.")→post_to_wall;
  }
  $URL := "HTTP://www.random.org/cgi-bin/randbyte?nbytes=" ∥ math→ceiling($bitsRequired / 8) ∥ "&format=b";
  if $showWorking→equals(true) then {
    "URL is:" →post_to_wall;
    $URL→post_to_wall;
  }
  // Download the required data from random.org, the remove spaces and trim surplus bits.
  $keyText := "";
  while $keyText→equals("") do {
    $keyText := web→download($URL)→replace_regex("[^01]", "")→substring(0, $bitsRequired);
  }
  if $showWorking→equals(true) then {
    "Key text is:" →post_to_wall;
    $keyText→post_to_wall;
  }
  foreach s in $keyText where true do {
    $keyBinary→add($s→to_number);
  }
  if $showWorking→equals(true) then {
    wall→create_text_box("The encryption key is:\n" ∥ $keyText, 36)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

action pre\u002DgenKey(bitsRequired: Number, showWorking: Boolean) returns keyBinary: Number_Collection {
  $keyBinary := collections→create_number_collection;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Pre-generated key");
    wall→set_reversed(true);
  }
  $keyText := wall→ask_string("Paste in the key data and press go.");
  if $showWorking→equals(true) then {
    ("Key text is " ∥ $keyText)→post_to_wall;
  }
  foreach s in $keyText where true do {
    $keyBinary→add($s→to_number);
  }
  if $showWorking→equals(true) then {
    wall→create_text_box("The key is:\n" ∥ $keyText, 36)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

action Generate_Key(bitsRequired: Number, showWorking: Boolean) returns key: String {
  // Generate the specified amount of key data.
  time→log(time→now ∥ " - Starting key generation.");
  $key := "";
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Generate Key");
    wall→set_reversed(true);
    ("Generating " ∥ $bitsRequired ∥ " key bits.")→post_to_wall;
  }
  $keyBinary := code→generateKey($bitsRequired, $showWorking);
  $key := ♻ Converter→binary\u002Dtext($keyBinary, $showWorking);
  if $showWorking→equals(true) then {
    ("Generated key is:\n" ∥ $key)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  time→log(time→now ∥ " - Key generation complete.");
}

action manualKey(bitsRequired: Number, showWorking: Boolean) returns keyBinary: Number_Collection {
  $keyBinary := collections→create_number_collection;
  $keyText := "";
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Manual Key");
    wall→set_reversed(true);
    ("Generating manual key of " ∥ $bitsRequired ∥ " bits.")→post_to_wall;
  }
  while $keyText→count < $bitsRequired do {
    skip;
  }
  if $showWorking→equals(true) then {
    ("Key text is " ∥ $keyText)→post_to_wall;
  }
  foreach s in $keyText where true do {
    $keyBinary→add($s→to_number);
  }
  $keyText→copy_to_clipboard;
  wall→prompt("The key data has been copied to the clipboard, make sure the recipient has a copy before sending your message.\n\nPress OK when you are ready to proceed.");
  if $showWorking→equals(true) then {
    wall→create_text_box("The encryption key is:\n" ∥ $keyText, 36)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

