action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");

    $a := $c->at("myKey");
    $b := $c->at("myKeyFalse");
}