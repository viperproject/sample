// Test case was used to compare precision of different numerical backward assignments (see report)

// Concretization picks x = 4 for which the assertion is violated :)

//:: ExpectedOutput(backward.counterexample.found)
action main(x: Number) {
    contract->assume(1 ≤ $x and $x ≤ 4);
    // simple glb: 1 <= x <= 4
    // linearization with prestate: 3.75 < x <= 4
    // exact solution: 3.87... <= x <= 4
    $x := $x * $x;
    // x > 15 and x <= 16

    //:: ExpectedOutput(assert.failed)
    contract->assert($x ≤ 15 , "does not hold");
}