action main() {
    $c := collections->create_number_collection();
    $c->add(2);
    $x := $c->at(0);
}