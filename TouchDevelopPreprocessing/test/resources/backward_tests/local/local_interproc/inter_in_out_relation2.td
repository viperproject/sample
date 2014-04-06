//:: ExpectedOutput(backward.counterexample.found)
action main(n: Number, i: Number) {
     $a := code→add($n, $i + 1);

     if ($a = 42) then {
        //:: ExpectedOutput(assert.failed)
        contract->assert(false, "never holds");
     }
}

action add(in: Number, incr: Number) returns (result: Number) {
    $result := $in + $incr;

    meta private;
}


