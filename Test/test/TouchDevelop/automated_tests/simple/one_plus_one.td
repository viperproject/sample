action main() {
    // let's see if sample/apron can do some elementary-school-level math
    $a := 1;
    $b := 1;

    // should work with constants
    contract->assert(1 + 1 = 2, "");

    // ... and variables
    contract->assert($a + $b = 2, "");

    //:: ExpectedOutput(assert.failed)
    contract->assert($a + $b = 3, "");
}