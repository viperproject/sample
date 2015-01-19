action foo(c: Number_Collection) {
    foreach e in $c where true do {
        $e->post_to_wall();
    }
}