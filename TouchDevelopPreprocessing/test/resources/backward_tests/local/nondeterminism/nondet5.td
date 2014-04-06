//:: ExpectedOutput(backward.counterexample.found)
action main() {
     for 0 ≤ i < 3 do {
        $x := wall->ask_number("num?");
        //:: ExpectedOutput(assert.failed)
        contract->assert ($x ≠ $i, "may not hold");
     }
}