action main() {
    $a := false;
    contract->assert(not $a, "must hold");
}