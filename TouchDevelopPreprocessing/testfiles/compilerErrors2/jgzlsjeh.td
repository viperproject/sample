meta version "v2.2,nothing";
meta name "One-Time Encryption";
meta icon "lock";
meta color "#ff007fff";
//
meta isLibrary "yes";
meta platform "current";
meta import menus {
  pub "leqgzvwl"
  usage {
    action showMenu(choices: String_Collection, prompt: String, caption: String) returns choiceIndex: Number
  }
}
meta import Converter {
  pub "omiywpeu"
  usage {
    action bin\u002Ddec(bin: String) returns dec: Number
    action dec\u002Dbin(dec: Number) returns bin: String
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
  while $bitsRequired > $pixels * 3 do {
    wall→prompt("The picture you gave chosen is too small to encrypt this message. Please choose another.");
    $keyPicture := media→choose_picture;
    $width := $keyPicture→width;
    $height := $keyPicture→height;
    $pixels := $width * $height;
  }
  if $showWorking→equals(true) then {
    "Key picture is:" →post_to_wall;
    $keyPicture→post_to_wall;
  }
  $steps := math→floor($pixels / $bitsRequired);
  for 0 ≤ i < math→ceiling($bitsRequired / 3) do {
    skip;
    $x := math→floor($i * $steps / $width);
    $y := $i * $steps / $height;
    $colour := $keyPicture→pixel($x, $y);
    if $showWorking→equals(true) then {
      ("Pixel " ∥ $i ∥ " (" ∥ $x ∥ "," ∥ $y ∥ ")")→post_to_wall;
      $colour→post_to_wall;
    }
    $keyBinary→add(math→round($colour→R));
    $keyBinary→add(math→round($colour→G));
    $keyBinary→add(math→round($colour→B));
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
  $output := collections→create_number_collection;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("XOR");
    wall→set_reversed(true);
    ("XOR Input 1:\n" ∥ $input1)→post_to_wall;
    ("XOR Input 2:\n" ∥ $input2)→post_to_wall;
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
    ("XOR output:\n" ∥ $input2)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

action textToBinary(text: String, showWorking: Boolean) returns binary: Number_Collection {
  $binary := collections→create_number_collection;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Text to binary");
    wall→set_reversed(true);
    ("Binary string:\n" ∥ $binary)→post_to_wall;
  }
  foreach s in $text where true do {
    $binary→add($s→to_number);
  }
  if $showWorking→equals(true) then {
    ("Binary data:\n" ∥ $binary)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

action binaryToText(binary: Number_Collection, showWorking: Boolean) returns text: String {
  $text := "";
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Binary to Text");
    wall→set_reversed(true);
    ("Binary data:\n" ∥ $binary)→post_to_wall;
  }
  foreach x in $binary where true do {
    $text := $text ∥ $x;
  }
  if $showWorking→equals(true) then {
    ("Binary string:\n" ∥ $text)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

action One\u002DTime_Encryption_Demo_() {
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
}

action Encrypt(clearText: String, showWorking: Boolean) returns cipherText: String {
  // Returns the clear text input as an encrypted binary string.
  $cipherText := "";
  time→log("Encrypting the following text:\n" ∥ $clearText);
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Encrypt");
    wall→set_reversed(true);
    "Clear text:" →post_to_wall;
    $clearText→post_to_wall;
  }
  // Convert the cleartext into unicode data.
  $unicode := code→textToUnicode($clearText, $showWorking);
  if $showWorking→equals(true) then {
    "Unicode data:" →post_to_wall;
  }
  // Convert the Unicode values to binary.
  $clearBinary := code→unicodeToBinary($unicode, $showWorking);
  if $showWorking→equals(true) then {
    "Clear binary:" →post_to_wall;
    $clearBinary→post_to_wall;
  }
  // Ask the user to choose an encryption key.
  // Generate the binary data from the key.
  $keyBinary := code→generateKey($clearBinary→count, $showWorking);
  if $showWorking→equals(true) then {
    "Key binary:" →post_to_wall;
    $keyBinary→post_to_wall;
  }
  code→binaryToText($keyBinary, $showWorking)→copy_to_clipboard;
  wall→prompt("The key data has been copied to the clipboard, make sure the recipient has a copy before sending your message.\n\nPress OK when you are ready to proceed.");
  // Combine the binary data with the encryption key.
  $cipherBinary := code→XOR($clearBinary, $keyBinary, $showWorking);
  if $showWorking→equals(true) then {
    "Cipher binary:" →post_to_wall;
    $cipherBinary→post_to_wall;
  }
  // Convert the binary data into a string.
  $cipherText := code→binaryToText($cipherBinary, $showWorking);
  if $showWorking→equals(true) then {
    "Cipher text:" →post_to_wall;
    $cipherText→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  time→log("Encrypted data:\n" ∥ $cipherText);
}

action Decrypt(cipherText: String, showWorking: Boolean) returns clearText: String {
  // Returns the encrypted binary input as clear text.
  $clearText := "";
  time→log("Decrypting the following data:\n" ∥ $cipherText);
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Decrypt");
    wall→set_reversed(true);
    "Cipher text:" →post_to_wall;
    $cipherText→post_to_wall;
  }
  // Generate the binary data from the key.
  $keyBinary := code→generateKey($cipherText→count, $showWorking);
  if $showWorking→equals(true) then {
    "Key binary:" →post_to_wall;
    $keyBinary→post_to_wall;
  }
  // Split the binary string into a collection of binary digits.
  $cipherBinary := code→textToBinary($cipherText, $showWorking);
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
  $clearUnicode := code→binaryToUnicode($clearBinary, $showWorking);
  if $showWorking→equals(true) then {
    "Unicode data:" →post_to_wall;
    $clearUnicode→post_to_wall;
  }
  // Read the text from the Unicode binary data.
  $clearText := code→unicodeToText($clearUnicode, $showWorking);
  if $showWorking→equals(true) then {
    "Clear text:" →post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  time→log("Decrypted data:\n" ∥ $clearText);
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
  $keyTypeChoice := ♻ menus→showMenu($keyTypes, $keyPrompt, "Choose key type");
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
  $key := "";
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Generate Key");
    wall→set_reversed(true);
    ("Generating " ∥ $bitsRequired ∥ " key bits.")→post_to_wall;
  }
  $keyBinary := code→generateKey($bitsRequired, $showWorking);
  $key := code→binaryToText($keyBinary, $showWorking);
  if $showWorking→equals(true) then {
    ("Generated key is:\n" ∥ $key)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
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

action textToUnicode(text: String, showWorking: Boolean) returns unicode: Number_Collection {
  $unicode := collections→create_number_collection;
  $chars := $text→count;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Text to Unicode");
    wall→set_reversed(true);
    "Text:" →post_to_wall;
    $text→post_to_wall;
  }
  for 0 ≤ i < $text→count do {
    $char := $text→at($i);
    $unicodeValue := ($char→to_unicode);
    if $showWorking→equals(true) then {
      ("Char " ∥ $i + 1 ∥ ": " ∥ $char ∥ " = " ∥ $char→to_unicode ∥ " in Unicode")→post_to_wall;
    }
    $unicode→add($unicodeValue);
  }
  if $showWorking→equals(true) then {
    "Unicode data:" →post_to_wall;
    $unicode→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

action unicodeToBinary(unicode: Number_Collection, showWorking: Boolean) returns binary: Number_Collection {
  $binary := collections→create_number_collection;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Unicode to binary");
    wall→set_reversed(true);
    "Unicode data:" →post_to_wall;
    $unicode→post_to_wall;
  }
  foreach x1 in $unicode where true do {
    $byte := ♻ Converter→dec\u002Dbin($x1);
    if $showWorking→equals(true) then {
      ("Unicode " ∥ $x1 ∥ " = " ∥ $byte ∥ " in binary")→post_to_wall;
    }
    while $byte→count < 8 do {
      $byte := 0 ∥ $byte;
    }
    foreach s in $byte where true do {
      $binary→add($s→to_number);
    }
  }
  if $showWorking→equals(true) then {
    "Binary data:" →post_to_wall;
    $binary→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}

action binaryToUnicode(binary: Number_Collection, showWorking: Boolean) returns unicode: Number_Collection {
  $unicode := collections→create_number_collection;
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Binary to Unicode");
    wall→set_reversed(true);
    ("Unicode data:\n" ∥ $unicode)→post_to_wall;
  }
  for 0 ≤ i < $binary→count / 8 do {
    $byte := "";
    for 0 ≤ i1 < 8 do {
      $byte := $byte ∥ $binary→at((8 * $i) + $i1);
    }
    if $showWorking→equals(true) then {
      ("Byte " ∥ $i + 1 ∥ ": " ∥ $byte)→post_to_wall;
    }
    $dec := ♻ Converter→bin\u002Ddec($byte);
    $unicode→add($dec);
    if $showWorking→equals(true) then {
      ("Unicode data:\n" ∥ $dec)→post_to_wall;
      wall→prompt("Press OK to continue.");
      wall→pop_page;
    }
  }
  meta private;
}

action unicodeToText(unicode: Number_Collection, showWorking: Boolean) returns text: String {
  $text := "";
  if $showWorking→equals(true) then {
    wall→push_new_page;
    wall→set_title("Unicode to text");
    wall→set_reversed(true);
    ("Unicode data:\n" ∥ $unicode)→post_to_wall;
  }
  foreach x1 in $unicode where true do {
    $char := $x1→to_character;
    $text := $text ∥ $char;
    if $showWorking→equals(true) then {
      ("Unicode " ∥ $x1 ∥ " = " ∥ $char)→post_to_wall;
    }
  }
  if $showWorking→equals(true) then {
    ("Text string:\n" ∥ $text)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
  meta private;
}
var breadcrumb : String_Collection {
  // Records the location within the script to display on wall pages.
}

