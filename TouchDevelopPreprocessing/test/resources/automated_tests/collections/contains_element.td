action main() {
    $c := collections->create_number_collection();
    $c->add(2);
    $b1 := $c->contains(2);
    $b2 := $c->contains(3);
}