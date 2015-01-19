action main() {
    $c := collections->create_number_collection();
    $c->add(2);
    $c->add(3);

    $mx := $c->max();
    $mn := $c->min();
    $a := $c->avg();
}