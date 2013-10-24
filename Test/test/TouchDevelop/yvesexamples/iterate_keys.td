action main(){
    $s := collections->create_string_map();
    $s->set_at("myKey", "myValue");
    $s->set_at("myKey2", "myValue2");

    foreach e in $s->keys() where true do {
        $e->post_to_wall();
    }
}