action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");
    $c->remove("myKey");
}