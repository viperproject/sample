//:: IgnoreFile(/TouchBoost/issue/32/)
action main() {
    $res1 := code->fib(3);
    $res2 := code->fib(4);
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
