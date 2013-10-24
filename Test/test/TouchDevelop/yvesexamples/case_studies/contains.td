action main(c: String_Map) {
    if ($c->keys()->contains("Zurich")) then {
        $c->at("Zurich")->post_to_wall();
    }
}