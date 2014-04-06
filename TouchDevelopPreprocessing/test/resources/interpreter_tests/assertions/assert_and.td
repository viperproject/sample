action main() {
    $a := true;
    $b := true;
    contract->assert($a and $b, "holds");
}