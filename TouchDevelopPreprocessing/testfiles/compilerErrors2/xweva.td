meta version "v2.2,nothing";
meta name "sha1";
//
meta isLibrary "yes";
meta platform "";
meta import collection_util {
  pub "kdrv"
  usage {
    action allocate(n: Number) returns v: Number_Collection
    action clone(v: Number_Collection) returns r: Number_Collection
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import binary_operators {
  pub "juwd"
  usage {
    action @and(a: Number, b: Number) returns r: Number
    action @or(a: Number, b: Number) returns r: Number
    action xor(a: Number, b: Number) returns r: Number
    action @not(a: Number) returns r: Number
    action shl(value: Number, n: Number) returns r: Number
    action shr(value: Number, n: Number) returns r: Number
  }
}

action calculate(blob: Number_Collection) returns hash: Number_Collection {
  $len := $blob→count * 8;
  $buffer := ♻ collection_util→clone($blob);
  $buffer→add(128);
  // extends buffer
  for 0 ≤ j < math→mod($buffer→count, 64) - 56 do {
    $buffer→add(0);
  }
  for 0 ≤ i < 56 - math→mod($buffer→count, 64) do {
    $buffer→add(0);
  }
  $buffer→add(♻ binary_operators→@and(♻ binary_operators→shr($len, 52), 255));
  $buffer→add(♻ binary_operators→@and(♻ binary_operators→shr($len, 48), 255));
  $buffer→add(♻ binary_operators→@and(♻ binary_operators→shr($len, 40), 255));
  $buffer→add(♻ binary_operators→@and(♻ binary_operators→shr($len, 32), 255));
  $buffer→add(♻ binary_operators→@and(♻ binary_operators→shr($len, 24), 255));
  $buffer→add(♻ binary_operators→@and(♻ binary_operators→shr($len, 16), 255));
  $buffer→add(♻ binary_operators→@and(♻ binary_operators→shr($len, 8), 255));
  $buffer→add(♻ binary_operators→@and($len, 255));
  // calc digest
  $h0 := 1732584193;
  $h1 := 4023233417;
  $h2 := 2562383102;
  $h3 := 271733878;
  $h4 := 3285377520;
  $index := 0;
  while $index < $buffer→count do {
    $W := ♻ collection_util→allocate(80);
    // Step A
    for 0 ≤ k < 16 do {
      $W→set_at($k, ♻ binary_operators→@or(♻ binary_operators→@or(♻ binary_operators→shl($buffer→at($k * 4 + $index), 24), ♻ binary_operators→shl($buffer→at($k * 4 + 1 + $index), 16)), ♻ binary_operators→@or(♻ binary_operators→shl($buffer→at($k * 4 + 2 + $index), 8), $buffer→at($k * 4 + 3 + $index))));
    }
    // Step B
    for 0 ≤ l < 64 do {
      $value := ♻ binary_operators→xor(♻ binary_operators→xor(♻ binary_operators→xor($W→at($l + 13), $W→at($l + 8)), $W→at($l + 2)), $W→at($l));
      $W→set_at($l + 16, code→circular_shift($value, 1));
    }
    // Step C
    $a := $h0;
    $b := $h1;
    $c := $h2;
    $d := $h3;
    $e := $h4;
    // Step D
    for 0 ≤ m < 20 do {
      $x := ♻ binary_operators→@and(code→circular_shift($a, 5) + code→f00($b, $c, $d) + $e + $W→at($m) + 1518500249, 4294967295);
      $e := $d;
      $d := $c;
      $c := code→circular_shift($b, 30);
      $b := $a;
      $a := $x;
    }
    for 0 ≤ m1 < 20 do {
      $x1 := ♻ binary_operators→@and(code→circular_shift($a, 5) + code→f20($b, $c, $d) + $e + $W→at($m1 + 20) + 1859775393, 4294967295);
      $e := $d;
      $d := $c;
      $c := code→circular_shift($b, 30);
      $b := $a;
      $a := $x1;
    }
    for 0 ≤ m2 < 20 do {
      $x2 := ♻ binary_operators→@and(code→circular_shift($a, 5) + code→f40($b, $c, $d) + $e + $W→at($m2 + 40) + 2400959708, 4294967295);
      $e := $d;
      $d := $c;
      $c := code→circular_shift($b, 30);
      $b := $a;
      $a := $x2;
    }
    for 0 ≤ m3 < 20 do {
      $x3 := ♻ binary_operators→@and(code→circular_shift($a, 5) + code→f60($b, $c, $d) + $e + $W→at($m3 + 60) + 3395469782, 4294967295);
      $e := $d;
      $d := $c;
      $c := code→circular_shift($b, 30);
      $b := $a;
      $a := $x3;
    }
    // Step E
    $h0 := ♻ binary_operators→@and($h0 + $a, 4294967295);
    $h1 := ♻ binary_operators→@and($h1 + $b, 4294967295);
    $h2 := ♻ binary_operators→@and($h2 + $c, 4294967295);
    $h3 := ♻ binary_operators→@and($h3 + $d, 4294967295);
    $h4 := ♻ binary_operators→@and($h4 + $e, 4294967295);
    $index := $index + 64;
  }
  $hash := collections→create_number_collection;
  $hash→add_many(code→ulong_binary($h0));
  $hash→add_many(code→ulong_binary($h1));
  $hash→add_many(code→ulong_binary($h2));
  $hash→add_many(code→ulong_binary($h3));
  $hash→add_many(code→ulong_binary($h4));
}

action f00(B: Number, C: Number, D: Number) returns r: Number {
  $r := ♻ binary_operators→@or(♻ binary_operators→@and($B, $C), ♻ binary_operators→@and(♻ binary_operators→@not($B), $D));
  meta private;
}

action f20(B: Number, C: Number, D: Number) returns r: Number {
  $r := ♻ binary_operators→xor(♻ binary_operators→xor($B, $C), $D);
  meta private;
}

action f40(B: Number, C: Number, D: Number) returns r: Number {
  $r := ♻ binary_operators→@or(♻ binary_operators→@or(♻ binary_operators→@and($B, $C), ♻ binary_operators→@and($B, $D)), ♻ binary_operators→@and($C, $D));
  meta private;
}

action f60(B: Number, C: Number, D: Number) returns r: Number {
  $r := code→f20($B, $C, $D);
  meta private;
}

action circular_shift(value: Number, n: Number) returns r: Number {
  $r := ♻ binary_operators→@or(♻ binary_operators→shl($value, $n), ♻ binary_operators→shr($value, 32 - $n));
  meta private;
}

action ulong_binary(p: Number) returns v: Number_Collection {
  $v := collections→create_number_collection;
  $v→add(♻ binary_operators→@and(♻ binary_operators→shr($p, 24), 255));
  $v→add(♻ binary_operators→@and(♻ binary_operators→shr($p, 16), 255));
  $v→add(♻ binary_operators→@and(♻ binary_operators→shr($p, 8), 255));
  $v→add(♻ binary_operators→@and(♻ binary_operators→shr($p, 0), 255));
  meta private;
}

