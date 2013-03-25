meta version "v2.2,nothing";
meta name "Converter";
meta icon "recycle";
meta color "#ff007fff";
// Convert between binary, decimal, hex, text and Unicode.
meta isLibrary "yes";
meta platform "current";

action Converter() {
  wall→set_title("Converter");
  wall→set_reversed(true);
  "This script is a library, use it in your other scripts." →post_to_wall;
  "Some functions take a \"debug\" parameter, set it to true to show the working." →post_to_wall;
}

action decimal\u002Dhex(dec: Number) returns hex: String {
  // Returns the given decimal number as a hexadecimal string.
  $hex := "";
  $hexDigits := "A,B,C,D,E,F" →split(",");
  $quotient := math→floor($dec / 16);
  if $quotient > 0 then {
    $hex := $hex ∥ code→decimal\u002Dhex($quotient);
  }
  $remainder := math→mod($dec, 16)→to_string;
  if $remainder→count > 1 then {
    $remainder := $hexDigits→at($remainder→to_number - 10);
  }
  $hex := $hex ∥ $remainder;
}

action binary\u002Ddecimal(bin: String) returns dec: Number {
  // Returns the given binary string as a decimal number.
  $dec := 0;
  for 0 ≤ digit < $bin→count do {
    $position := ($bin→count) - $digit;
    $bit := $bin→at($position - 1)→to_number;
    $dec := $dec + ($bit * (math→pow(2, $digit)));
  }
}

action decimal\u002Dbinary(dec: Number) returns bin: String {
  // Returns the given decimal number a binary string.
  $bin := "";
  $quotient := math→floor($dec / 2);
  if $quotient > 0 then {
    $bin := $bin ∥ code→decimal\u002Dbinary($quotient);
  }
  $remainder := math→mod($dec, 2)→to_string;
  $bin := $bin ∥ $remainder;
}

action hex\u002Ddecimal(hex: String) returns dec: Number {
  // Returns the given hexadecimal string as a decimal number.
  $dec := 0;
  $hexDigits := "A,B,C,D,E,F" →split(",");
  for 0 ≤ i < $hex→count do {
    $position := ($hex→count) - $i - 1;
    $bit := $hex→at($position);
    if $hex→at($position)→is_match_regex("[^0-9]") then {
      $bit := ($hexDigits→index_of($bit, 0) + 10)→to_string;
    }
    $dec := $dec + ($bit→to_number * (math→pow(16, $i)));
  }
}

action binary\u002Dhex(bin: String) returns hex: String {
  // Returns the given binary string as a hecadecimal string.
  $hex := code→decimal\u002Dhex(code→binary\u002Ddecimal($bin));
}

action hex\u002Dbinary(hex: String) returns bin: String {
  // Returns the given hexadecimal string as a binary string.
  $bin := code→decimal\u002Dbinary(code→hex\u002Ddecimal($hex));
}

action binary\u002DUnicode(binary: Number_Collection, debug: Boolean) returns unicode: Number_Collection {
  // Returns the given array of binary values as an array of Unicode values.
  $unicode := collections→create_number_collection;
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Binary to Unicode");
    wall→set_reversed(true);
    "Binary data:" →post_to_wall;
    $binary→post_to_wall;
  }
  for 0 ≤ unicodeByte < $binary→count / 8 do {
    $byte := "";
    for 0 ≤ bit < 8 do {
      $byte := $byte ∥ $binary→at((8 * $unicodeByte) + $bit);
    }
    if $debug→equals(true) then {
      ("Byte " ∥ $unicodeByte + 1 ∥ ": " ∥ $byte)→post_to_wall;
    }
    $dec := code→binary\u002Ddecimal($byte);
    $unicode→add($dec);
  }
  if $debug→equals(true) then {
    ("Unicode data:")→post_to_wall;
    $unicode→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
}

action binary\u002Dtext(binary: Number_Collection, debug: Boolean) returns text: String {
  // Returns the given array of binary values as a continuous text string.
  $text := "";
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Binary to Text");
    wall→set_reversed(true);
    "Binary data:" →post_to_wall;
    $binary→post_to_wall;
  }
  foreach bit in $binary where true do {
    $text := $text ∥ $bit;
  }
  if $debug→equals(true) then {
    "Binary string:" →post_to_wall;
    $text→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
}

action text\u002Dbinary(text: String, debug: Boolean) returns binary: Number_Collection {
  // Returns the given binary string as an array of binary digits.
  $binary := collections→create_number_collection;
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Text to binary");
    wall→set_reversed(true);
    "Binary string:" →post_to_wall;
    $text→post_to_wall;
  }
  foreach bit in $text where true do {
    $binary→add($bit→to_number);
  }
  if $debug→equals(true) then {
    "Binary data:" →post_to_wall;
    $binary→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
}

action text\u002DUnicode(text: String, debug: Boolean) returns unicode: Number_Collection {
  // Returns the given text string as an array of Unicode values.
  $unicode := collections→create_number_collection;
  $chars := $text→count;
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Text to Unicode");
    wall→set_reversed(true);
    "Text:" →post_to_wall;
    $text→post_to_wall;
  }
  for 0 ≤ position < $text→count do {
    $char := $text→at($position);
    $unicodeValue := ($char→to_unicode);
    if $debug→equals(true) then {
      ("Char " ∥ $position + 1 ∥ ": " ∥ $char ∥ " = " ∥ $char→to_unicode ∥ " in Unicode")→post_to_wall;
    }
    $unicode→add($unicodeValue);
  }
  if $debug→equals(true) then {
    "Unicode data:" →post_to_wall;
    $unicode→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
}

action Unicode\u002Dbinary(unicode: Number_Collection, debug: Boolean) returns binary: Number_Collection {
  // Returns the given array of Unicode values an array of binary Unicode values.
  $binary := collections→create_number_collection;
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Unicode to binary");
    wall→set_reversed(true);
    "Unicode data:" →post_to_wall;
    $unicode→post_to_wall;
  }
  foreach unicodeByte in $unicode where true do {
    $byte := code→decimal\u002Dbinary($unicodeByte);
    while $byte→count < 8 do {
      $byte := 0 ∥ $byte;
    }
    foreach bit in $byte where true do {
      $binary→add($bit→to_number);
    }
    if $debug→equals(true) then {
      ("Unicode " ∥ $unicodeByte ∥ " = " ∥ $byte ∥ " in binary")→post_to_wall;
    }
  }
  if $debug→equals(true) then {
    "Binary data:" →post_to_wall;
    $binary→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
}

action Unicode\u002Dtext(unicode: Number_Collection, debug: Boolean) returns text: String {
  // Returns the given array of Unicode values as a text string.
  $text := "";
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Unicode to text");
    wall→set_reversed(true);
    "Unicode data:" →post_to_wall;
    $unicode→post_to_wall;
  }
  foreach unicodeByte in $unicode where true do {
    $char := $unicodeByte→to_character;
    $text := $text ∥ $char;
    if $debug→equals(true) then {
      ("Unicode " ∥ $unicodeByte ∥ " = " ∥ $char)→post_to_wall;
    }
  }
  if $debug→equals(true) then {
    "Text string:" →post_to_wall;
    $text→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
}

