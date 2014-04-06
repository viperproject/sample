// Case where we fail because of non-linear arithmetic operation

//:: MissingOutput(backward.counterexample.found, /sample/issue/0000/)
action main(x: Number) {
    //:: ExpectedOutput(assert.failed)
    contract->assert($x*$x ≠ 16 , "does not hold, x=4 is counterexample");
}