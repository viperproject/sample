action numberCollection() {
    $v := collections→create_number_collection;
    $v→add(1);
    $v→add(2);
    $x := 2;
    foreach e in $v do {
        $x := $x + 1;
        code->other();
        $e→post_to_wall;
    }
}

action other() {
    skip;
}