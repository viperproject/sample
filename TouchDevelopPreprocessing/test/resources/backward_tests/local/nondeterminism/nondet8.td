//:: ExpectedOutput(backward.counterexample.found)
action main() {
     $i := 0;
     while ($i < 3) do {
        $x := math->random(10);
        //:: ExpectedOutput(assert.failed)
        contract->assert ($x ≠ 0, "blub");
        $i := $i + 1;
     }
}