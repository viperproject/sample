action main() {
    $colors := collections->create_string_map();
    $colors->set_at("Black", "#000000");
    $colors->set_at("Red", "#FF0000");

    foreach c in $colors->keys() where true do {
        $x := $colors->at($c);
        $x->post_to_wall();
    }
}