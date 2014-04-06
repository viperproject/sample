//:: MissingOutput(backward.counterexample.found, /sample/issue/0000/)

// We cannot find a counter example since our backward analysis
// cannot infer anything about later loop iterations (with 1 time unfolding)
action main() {
     for 0 ≤ i < 3 do {
        $x := math->random($i+1);
        //:: ExpectedOutput(assert.failed)
        contract->assert ($x ≠ 2, "does not always hold");
     }
}