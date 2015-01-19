action main() {
    $c := collections->create_number_collection();
    $c->add(2);
    $c->add(3);
    $c->clear();
}