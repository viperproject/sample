action main() {
    $c := collections->create_string_collection();
    $c->add("myString");
    $b1 := $c->contains("myString");
    $b2 := $c->contains("myString2");
}