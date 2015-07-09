action main() {
    $c := collections->create_string_collection();
    $c->add("test");
    $c->at(0)->post_to_wall();
    contract->assert($c->count = 1, "Size 1");
}