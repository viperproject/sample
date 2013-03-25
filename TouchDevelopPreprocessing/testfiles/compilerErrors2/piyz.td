meta version "v2.2,nothing";
meta name "vibrato morse";
meta icon "loadingalt";
meta color "#ffff91a4";
// Converts words into Morse vibrations
meta platform "current";

action main() {
  // Converts a text into Morse code as vibrations.
  $text := wall→ask_string("Translate to Morse code");
  $codes := ".-!-...!-.-.!-..!.!..-.!--.!....!..!.---!-.-!.-..!--!-.!---!.--.!--.-!.-.!...!-!..-!...-!.--!-..-!-.--!--.." →split("!");
  foreach letter in $text→to_lower_case where true do {
    code→vibrate_letter($codes, $letter);
  }
}

action vibrate_code(code1: String) {
  // Converts a Morse code to vibrations.
  $code1→post_to_wall;
  foreach x1 in $code1 where true do {
    if $x1→equals(".") then {
      phone→vibrate(0.08);
    }
    else {
      phone→vibrate(0.25);
    }
    time→sleep(0.40);
  }
  meta private;
}

action vibrate_letter(codes: String_Collection, letter: String) {
  // Converts a letter to Morse vibrations.
  $c := $letter→to_unicode;
  time→fail_if_not($c→to_character→equals($letter));
  $x := "a" →to_unicode;
  if $x ≤ $c and $c ≤ $x + 26 then {
    $code1 := $codes→at($c - $x);
    code→vibrate_code($code1);
  }
  time→sleep(0.5);
  meta private;
}

