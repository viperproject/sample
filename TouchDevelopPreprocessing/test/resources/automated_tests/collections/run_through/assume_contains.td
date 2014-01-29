action foo(
    c:String_Map
) {
    if ($c->keys()->contains("bar")) then {
        $x := $c->at("bar");
        $x->post_to_wall();
    } else {
        $y := $c->at("bar");
        $y->post_to_wall();
    }
}