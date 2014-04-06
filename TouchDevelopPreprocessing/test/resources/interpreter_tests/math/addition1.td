action main() {
    $a := 1;
    $b := 2;
    $c := $a + $b;
    contract->assert($c = 3, "must hold");
}