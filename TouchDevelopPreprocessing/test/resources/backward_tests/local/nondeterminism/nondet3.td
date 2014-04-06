//:: ExpectedOutput(backward.counterexample.found)
action main() {
     for 0 ≤ i < 3 do {
        $x := math->random(2);
        $y := math->random(2);
        //:: ExpectedOutput(assert.failed)
        contract->assert ($x - $y ≠ 0, "does not always hold");
     }
}