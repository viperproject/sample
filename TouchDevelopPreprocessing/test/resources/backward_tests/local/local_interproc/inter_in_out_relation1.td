//:: ExpectedOutput(backward.counterexample.found)
action main(n: Number) {
     $a := code→increase($n+1);
     if ($a = 42) then {
        //:: ExpectedOutput(assert.failed)
        contract->assert(false, "never holds");
     }
}

action increase(in: Number) returns (result: Number) {
    $result := $in + 7;

    meta private;
}


