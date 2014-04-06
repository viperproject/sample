action main() {
    $a := true;
    $b := false;
    //:: ExpectedOutput(assert.failed)
    contract->assert($a and $b, "doesnt hold");
}