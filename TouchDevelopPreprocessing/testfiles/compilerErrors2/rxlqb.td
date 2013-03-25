meta version "v2.2,nothing";
meta name "DateTime";
meta icon "appointment";
// Date and time functions
meta isLibrary "yes";
meta platform "current";

action formatDate(dt1: DateTime) returns s1: String {
  $s1 := code→formatDate2($dt1, ".", ".", 321);
}

action formatTime(dt1: DateTime) returns s1: String {
  $s1 := code→formatTime2($dt1, ":");
}

action formatTime2(time1: DateTime, separator: String) returns s2: String {
  $s2 := code→formatPart($time1→hour);
  $s2 := $s2 ∥ $separator;
  $s2 := $s2 ∥ code→formatPart($time1→minute);
}

action formatPart(x1: Number) returns s1: String {
  if $x1 < 10 then {
    $s1 := "0";
  }
  $s1 := $s1 ∥ $x1→to_string;
  meta private;
}

action formatDate2(date: DateTime, separator1: String, separator2: String, sequence: Number) returns s1: String {
  $num := 0;
  $s1 := "";
  foreach s in $sequence→to_string where true do {
    if $num = 1 then {
      $s1 := $s1 ∥ $separator1;
    }
    else {
      if $num = 2 then {
        $s1 := $s1 ∥ $separator2;
      }
    }
    $num := $num + 1;
    if $s→equals("1") then {
      $s1 := $s1 ∥ code→formatPart($date→day);
    }
    else {
      if $s→equals("2") then {
        $s1 := $s1 ∥ code→formatPart($date→month);
      }
      else {
        if $s→equals("3") then {
          $s1 := $s1 ∥ code→formatPart($date→year);
        }
      }
    }
  }
}

