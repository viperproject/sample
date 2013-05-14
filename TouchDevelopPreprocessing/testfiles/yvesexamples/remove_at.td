action main() {
    $c := collections->create_string_collection();
    $c->add("a");
    $c->add("b");
    $c->remove_at(0);
}