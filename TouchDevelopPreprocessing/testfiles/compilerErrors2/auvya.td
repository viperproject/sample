meta version "v2.2,nothing";
meta name "collection util";
//
meta isLibrary "yes";
meta platform "current";
meta import binary_operators {
  pub "juwd"
  usage {
    action @and(a: Number, b: Number) returns r: Number
    action @or(a: Number, b: Number) returns r: Number
    action shr(value: Number, n: Number) returns r: Number
  }
}

action allocate(n: Number) returns v: Number_Collection {
  $v := collections→create_number_collection;
  for 0 ≤ i < $n do {
    $v→add(0);
  }
}

action hex_to_int(s: String) returns r: Number {
  $@table := "0123456789abcdef";
  $r := 0;
  foreach e in $s where true do {
    $r := $r * 16 + $@table→index_of($e→to_lower_case, 0);
  }
  meta private;
}

action to_hex_string(v: Number_Collection) returns s: String {
  $s := "";
  foreach e in $v where true do {
    $s := $s ∥ code→int_to_hex($e);
  }
}

action int_to_hex(p: Number) returns s: String {
  $@table := "0123456789abcdef";
  $s := $@table→at(math→floor($p / 16)) ∥ $@table→at(math→mod($p, 16));
  meta private;
}

action to_string(v: Number_Collection) returns s: String {
  $s := "";
  foreach e in $v where true do {
    $s := $s ∥ $e→to_character;
  }
}

action go() {
  skip;
  meta private;
}

action from_string(s: String) returns v: Number_Collection {
  $v := collections→create_number_collection;
  foreach e in $s where true do {
    $u16 := $e→to_unicode;
    if $u16 < 128 then {
      $v→add($u16);
    }
    else {
      if $u16 < 2048 then {
        $v→add(♻ binary_operators→@or(♻ binary_operators→@and(♻ binary_operators→shr($u16, 6), 31), 192));
      }
      else {
        $v→add(♻ binary_operators→@or(♻ binary_operators→shr($u16, 12), 224));
        $v→add(♻ binary_operators→@or(♻ binary_operators→@and(♻ binary_operators→shr($u16, 6), 63), 128));
      }
      $v→add(♻ binary_operators→@or(♻ binary_operators→@and($u16, 63), 128));
    }
  }
}

