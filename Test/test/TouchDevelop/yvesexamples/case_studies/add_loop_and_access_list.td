action main() {
    $c := collections->create_number_collection();

    for 0 ≤ i < 10 do {
        $c->add($i);
    }

    $c->at(1)->post_to_wall();
}