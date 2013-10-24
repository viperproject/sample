action main() {
    $c := collections->create_number_map();

    for 0 ≤ i < 10 do {
        $c->set_at($i, $i+1);
    }

    $x := 4 / $c->at(1);
}