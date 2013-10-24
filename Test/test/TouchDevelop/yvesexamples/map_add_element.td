action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");
    $x := $c->at("myKey");
    $x->post_to_wall();
}