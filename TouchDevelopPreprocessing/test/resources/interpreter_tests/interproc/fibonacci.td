action main() returns  {
  $result := code->fib(7);
  contract->assert($result = 13, "must hold");
}

action fib(n: Number) returns (r: Number) {
  if ($n = 0) then {
    $r := 0;
  } else {
      if ($n = 1) then {
        $r := 1;
      } else {
        $r := code->fib($n-1) + code->fib($n-2);
      }
  }
}