action main() {
    $c := collections->create_number_collection();
    $c->add(10);
    for 0 ≤ i < 5 do {
        $c->add($i);
    }
}