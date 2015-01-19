action main() {
    $c := collections->create_string_collection();
    $c->add("c");
    $c->add("a");
    $c->add("b");
    $c->sort();
}