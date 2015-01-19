action main() {
    $c := collections->create_string_collection();
    $c->add("myString");
    $c->add("myString2");
    $x := $c->index_of("myString", 0);
}