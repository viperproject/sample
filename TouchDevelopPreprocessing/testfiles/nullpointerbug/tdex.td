meta version "v2.2,nothing";
meta name "isbn";
// #rbhs13isbn
meta platform "current";

action main() {
  $s := wall→ask_string("");
  $total := 0;
  $at := 0;
  $x := 0;
  if $s→count = 10 then {
    code→check($s, $at, $total, $x);
  }
  else {
    "Invalid" →post_to_wall;
  }
}

action check(s: String, at: Number, total: Number, x: Number) {
  $is_invalid := false;
  for 0 ≤ i < 10 do {
    $at := $s→at($i)→to_number;
    if $s→at(9)→contains("X") or $s→at(9)→contains("x") then {
      $at := 10;
    }
    if $at→is_invalid then {
      $is_invalid := true;
    }
  }
  if math→mod($total, 11) = 0 and not $is_invalid then {
    "Valid" →post_to_wall;
  }
  else {
    "Invalid" →post_to_wall;
  }
  meta private;
}

