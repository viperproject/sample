//:: ExpectedOutput(backward.counterexample.found)
action main() {
     $x := math->random(2);
     //:: ExpectedOutput(assert.failed)
     contract->assert ($x ≠ 0, "does not always hold");
}