action main() {
    $c := collections->create_string_collection();
    $c->add("test");
    $d := collections->create_string_collection();

    foreach s in $c where true do {
        $d->add($s);
    }

    contract->assert($d->count ≤ $c->count, "Same size");
}