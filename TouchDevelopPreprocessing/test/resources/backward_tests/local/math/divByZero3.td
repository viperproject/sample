//:: ExpectedOutput(backward.counterexample.found)
action main(a: Number, b: Number, c: Number) {
     if ($a = $b and $b = $c) then {
        //:: ExpectedOutput(assert.failed)
        $r := 1 / ($a - $c);
     }
}