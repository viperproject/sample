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
  if data→hexDigits→is_invalid then {
    data→hexDigits := "A,B,C,D,E,F" →split(",");
  }
  $hex := "";
  $quotient := math→floor($dec / 16);
  if $quotient > 0 then {
    $hex := $hex ∥ code→decimal\u002Dhex($quotient);
  }
  $remainder := math→mod($dec, 16)→to_string;
  if $remainder→count > 1 then {
    $remainder := data→hexDigits→at($remainder→to_number - 10);
  }
  $hex := $hex ∥ $remainder;
}

action binary\u002Ddecimal(bin: String) returns dec: Number {
  $dec := 0;
  for 0 ≤ i < $bin→count do {
    $position := ($bin→count) - $i;
    $bit := $bin→at($position - 1)→to_number;
    $dec := $dec + ($bit * (math→pow(2, $i)));
  }
}

action decimal\u002Dbinary(dec: Number) returns bin: String {
  $bin := "";
  $quotient := math→floor($dec / 2);
  if $quotient > 0 then {
    $bin := $bin ∥ code→decimal\u002Dbinary($quotient);
  }
  $remainder := math→mod($dec, 2)→to_string;
  $bin := $bin ∥ $remainder;
}

action hex\u002Ddecimal(hex: String) returns dec: Number {
  $dec := 0;
  if data→hexDigits→is_invalid then {
    data→hexDigits := "A,B,C,D,E,F" →split(",");
  }
  for 0 ≤ i < $hex→count do {
    $position := ($hex→count) - $i - 1;
    $bit := $hex→at($position);
    if $hex→at($position)→is_match_regex("[^0-9]") then {
      $bit := (data→hexDigits→index_of($bit, 0) + 10)→to_string;
    }
    $dec := $dec + ($bit→to_number * (math→pow(16, $i)));
  }
}
var hexDigits : String_Collection {
}

action binary\u002Dhex(bin: String) returns hex: String {
  $hex := code→decimal\u002Dhex(code→binary\u002Ddecimal($bin));
}

action hex\u002Dbinary(hex: String) returns bin: String {
  $bin := code→decimal\u002Dbinary(code→hex\u002Ddecimal($hex));
}

action binary\u002DUnicode(binary: Number_Collection, debug: Boolean) returns unicode: Number_Collection {
  $unicode := collections→create_number_collection;
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Binary to Unicode");
    wall→set_reversed(true);
    "Binary data:" →post_to_wall;
    $binary→post_to_wall;
  }
  for 0 ≤ i < $binary→count / 8 do {
    $byte := "";
    for 0 ≤ i1 < 8 do {
      $byte := $byte ∥ $binary→at((8 * $i) + $i1);
    }
    if $debug→equals(true) then {
      ("Byte " ∥ $i + 1 ∥ ": " ∥ $byte)→post_to_wall;
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
  $text := "";
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Binary to Text");
    wall→set_reversed(true);
    "Binary data:" →post_to_wall;
    $binary→post_to_wall;
  }
  foreach x in $binary where true do {
    $text := $text ∥ $x;
  }
  if $debug→equals(true) then {
    "Binary string:" →post_to_wall;
    $text→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
}

action text\u002Dbinary(text: String, debug: Boolean) returns binary: Number_Collection {
  $binary := collections→create_number_collection;
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Text to binary");
    wall→set_reversed(true);
    "Binary string:" →post_to_wall;
    $text→post_to_wall;
  }
  foreach s in $text where true do {
    $binary→add($s→to_number);
  }
  if $debug→equals(true) then {
    "Binary data:" →post_to_wall;
    $binary→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
}

action text\u002DUnicode(text: String, debug: Boolean) returns unicode: Number_Collection {
  $unicode := collections→create_number_collection;
  $chars := $text→count;
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Text to Unicode");
    wall→set_reversed(true);
    "Text:" →post_to_wall;
    $text→post_to_wall;
  }
  for 0 ≤ i < $text→count do {
    $char := $text→at($i);
    $unicodeValue := ($char→to_unicode);
    if $debug→equals(true) then {
      ("Char " ∥ $i + 1 ∥ ": " ∥ $char ∥ " = " ∥ $char→to_unicode ∥ " in Unicode")→post_to_wall;
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
  $binary := collections→create_number_collection;
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Unicode to binary");
    wall→set_reversed(true);
    "Unicode data:" →post_to_wall;
    $unicode→post_to_wall;
  }
  foreach x in $unicode where true do {
    $byte := code→decimal\u002Dbinary($x);
    while $byte→count < 8 do {
      $byte := 0 ∥ $byte;
    }
    foreach s in $byte where true do {
      $binary→add($s→to_number);
    }
    if $debug→equals(true) then {
      ("Unicode " ∥ $x ∥ " = " ∥ $byte ∥ " in binary")→post_to_wall;
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
  $text := "";
  if $debug→equals(true) then {
    wall→push_new_page;
    wall→set_title("Unicode to text");
    wall→set_reversed(true);
    ("Unicode data:\n" ∥ $unicode)→post_to_wall;
  }
  foreach x1 in $unicode where true do {
    $char := $x1→to_character;
    $text := $text ∥ $char;
    if $debug→equals(true) then {
      ("Unicode " ∥ $x1 ∥ " = " ∥ $char)→post_to_wall;
    }
  }
  if $debug→equals(true) then {
    ("Text string:\n" ∥ $text)→post_to_wall;
    wall→prompt("Press OK to continue.");
    wall→pop_page;
  }
}

