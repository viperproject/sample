action numberCollection() {
    $v := collections→create_number_collection;
    $v→add(1);
    $v→add(2);
    $x := 2;
    foreach e in $v where true do {
        $x := $x + 1;
        $e→post_to_wall;
    }
}