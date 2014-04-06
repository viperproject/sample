//:: ExpectedOutput(backward.counterexample.found)
action main(a: Number, b: Number, c: Number) {
     if ($a = 1 and $b > 5) then {
        //:: ExpectedOutput(assert.failed)
        $r := 1 / ($c-2);
     }
}