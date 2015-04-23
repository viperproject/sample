action main() {
    $s1 := code->create_coll();
    $s2 := code->create_coll();

    $s1->set_at("myKey", "myValue");
}

private action create_coll() returns(c: String_Map) {
    $c := collections->create_string_map();
}