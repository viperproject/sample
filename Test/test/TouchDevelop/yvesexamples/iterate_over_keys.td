action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");
    $c->set_at("myKey2", "myValue2");

    foreach k in $c->keys() where true do {
        $e := $c->at($k);
        $e->post_to_wall();
    }
}