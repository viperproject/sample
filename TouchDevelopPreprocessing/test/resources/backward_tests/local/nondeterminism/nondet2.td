//:: ExpectedOutput(backward.counterexample.found)
action main() {
     $x := math->random(2);
     $y := math->random(2);
     //:: ExpectedOutput(assert.failed)
     contract->assert ($x - $y ≠ 0, "does not always hold");
}