meta version "v2.2,nothing";
meta name "base conversion";
meta icon "calculator";
meta color "#ff008000";
// Converts any value from any base (1-36) to any other base (1-36)
meta platform "";

action main() {
  wall→set_title("Base conversion utility");
  $text := "Input base";
  $inBase := wall→ask_number($text→trim(" \t"));
  if $inBase < 1 or $inBase > 36 then {
    wall→prompt("Input base outside range 1-36");
  }
  else {
    $inNum := wall→ask_string("Enter your value to convert from");
    if code→maxValInNum($inNum) ≥ $inBase then {
      wall→prompt("Warning: input value contains digits representing values greater than the input base. Script will function, but may not make logical sense.");
    }
    $oBase := wall→ask_number("Enter your output base");
    if $oBase < 1 or $oBase > 36 then {
      wall→prompt("Output base outside range (1 - 36)");
    }
    else {
      $val := code→numToDec($inNum, $inBase);
      code→decToNum($val, $oBase)→post_to_wall;
    }
  }
}

action charToNum(c: String) returns num: Number {
  $num := 0;
  if $c→to_number→is_invalid then {
    $uni := $c→to_unicode;
    if $uni ≥ 65 and $uni ≤ 90 then {
      $num := $uni - 55;
    }
    else {
      if $uni ≥ 97 and $uni ≤ 122 then {
        $num := $uni - 87;
      }
      else {
        $num := - 1;
      }
    }
  }
  else {
    $num := $c→to_number;
  }
}

action numToDec(num: String, base: Number) returns val: Number {
  $val := 0;
  $length := $num→count;
  for 0 ≤ i < $length do {
    $val := $val + (math→pow($base, $i) * code→charToNum($num→at($length - ($i + 1))));
  }
}

action decToNum(val: Number, base: Number) returns num: String {
  $num := "";
  while $val > 0 do {
    $rem := 0;
    $val := ($val / $base);
    $rem := math→floor(($val - math→floor($val)) * $base);
    $val := math→floor($val);
    $num := (code→numToChar($rem) ∥ $num);
  }
}

action numToChar(val: Number) returns c: String {
  $alphanumeric := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  $c := $alphanumeric→at($val);
}

action maxValInNum(num: String) returns r: Number {
  $r := 0;
  foreach e in $num where true do {
    $r := math→max($r, code→charToNum($e));
  }
}

