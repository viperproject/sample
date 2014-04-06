//:: ExpectedOutput(backward.counterexample.found)
action main(n: Number, b: Boolean) {
     $a := 0;


     if ($b) then {
          $a := code→add($n, 2);
     } else {
          $a := code→add($n, - $n);
     }

     //:: ExpectedOutput(assert.failed)
     contract->assert($a < 10, "never holds");

}

action add(in: Number, incr: Number) returns (result: Number) {
    $result := $in + $incr;

    meta private;
}

