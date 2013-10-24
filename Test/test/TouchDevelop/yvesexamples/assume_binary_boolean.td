action foo(
    c:String_Map
) {
    if ($c->keys()->contains("bar") and $c->keys()->contains("foo")) then {
        $c->at("bar")->post_to_wall();
        $c->at("foo")->post_to_wall();
    }
}