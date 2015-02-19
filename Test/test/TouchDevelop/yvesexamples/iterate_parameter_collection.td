action foo(c: Collection_Number) {
    foreach e in $c where true do {
        $e->post_to_wall();
    }
}