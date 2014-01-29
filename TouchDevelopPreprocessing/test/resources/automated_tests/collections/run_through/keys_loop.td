action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");


    foreach e in $c->keys() where true do {
        $e->post_to_wall();
    }
}