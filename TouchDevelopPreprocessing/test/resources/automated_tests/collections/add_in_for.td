action main() {
    $c := collections->create_string_collection();

    for 0 ≤ i < 2 do {
        $c->add("test");
    }
    contract->assert($c->count ≤ 2, "Same size");
}