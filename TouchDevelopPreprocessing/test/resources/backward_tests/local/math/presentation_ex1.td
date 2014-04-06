//:: ExpectedOutput(backward.counterexample.found)
action main(x: Number, y: Number, t: Number) {
     $t := ($x - $y) - 1;
     if ($x ≥ $y) then {
        //:: ExpectedOutput(assert.failed)
        $r := 1 / $t;
     }
}