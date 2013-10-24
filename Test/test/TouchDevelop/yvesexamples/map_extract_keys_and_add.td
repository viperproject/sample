action main() {
    $c := collections->create_string_map();
    $c->set_at("key2", "value2");
    $k := $c->keys();

    $k->add("key");

    if($k->contains("key")) then {
        $x := $c->at("key");
        $x->post_to_wall();
    }
}