action main() {
    $c := collections->create_number_map();
    $c->set_at(100, 2);
    $c->set_at(200, 0);

    $x := $c->at(100);
    $y := 4 / $x;
}