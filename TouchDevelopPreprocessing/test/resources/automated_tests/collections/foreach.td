action main() {
    $c := collections->create_string_collection();
    $c->add("myValue");
    $c->add("myValue2");

    foreach e in $c where true do {
        $e->post_to_wall();
    }
}