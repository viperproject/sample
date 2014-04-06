//:: ExpectedOutput(backward.counterexample.found)
action main(x: Number, y: Number) {
    if ($x ≤ 100) then {
       $x := 2*$x + 16;
       //:: ExpectedOutput(assert.failed)
       contract->assert($x ≠ 0, "foo");
    }

}
