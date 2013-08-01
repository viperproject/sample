action main() {
    $c := collections->create_string_collection();
    $c->add("myString");
    $c->insert_at(0, "myString2");
}