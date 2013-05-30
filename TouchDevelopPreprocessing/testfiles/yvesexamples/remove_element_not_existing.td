action main() {
    $c := collections->create_string_collection();
    $c->add("myString");
    $b := $c->remove("myString2");
}