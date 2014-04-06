//:: ExpectedOutput(backward.counterexample.found)
action main(i: Number, j: Number, k: Number) {
    contract->assume($i ≥ 10 and $i ≤ 20);
    contract->assume($j ≥ 1 and $j ≤ 2);

    $k := $i + $j;

    //:: ExpectedOutput(assert.failed)
    contract->assert($k ≥ 15, "fails");
}