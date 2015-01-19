action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");
    $c->set_at("myKey2", "myValue2");


    foreach e in $c where true do {
        $e->post_to_wall();
    }
}