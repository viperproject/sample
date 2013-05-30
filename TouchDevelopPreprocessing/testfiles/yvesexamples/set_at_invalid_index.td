action main() {
    $c := collections->create_string_collection();
    $c->add("myString");
    $c->set_at(1, "myString2");
}