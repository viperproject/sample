action main() {
    $c := collections->create_number_collection();
    for 0 ≤ i < 5 do {
        $c->add($i);
    }

    $x := $c->at(3);
    $x->post_to_wall();

    $y := $c->at(10);
    $y->post_to_wall();
}