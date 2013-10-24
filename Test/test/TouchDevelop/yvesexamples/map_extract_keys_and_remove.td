action main() {
    $c := collections->create_string_map();

    $c->set_at("key", "value");
    $k := $c->keys();

    $c->remove("key");

    if($k->contains("key")) then {
        $x := $c->at("key");
        $x->post_to_wall();
    }
}