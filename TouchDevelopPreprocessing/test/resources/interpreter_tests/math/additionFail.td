action main() {
    $a := 1;
    $b := 1;
    $c := $a + $b;
    //:: ExpectedOutput(assert.failed)
    contract->assert($c = 3, "must hold");
}