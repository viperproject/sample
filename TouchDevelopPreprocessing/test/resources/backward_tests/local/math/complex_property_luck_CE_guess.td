// Backward analysis yields top, but we're lucky because we pick 0 or 1 first :)

//:: ExpectedOutput(backward.counterexample.found)
action main(x: Number) {
    //:: ExpectedOutput(assert.failed)
    contract->assert($x*$x ≠ $x , "does not hold, x=0, x=1 are counterexamples");
}