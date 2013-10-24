action main(c: String_Map) {
    $c->remove("Zurich");
    if($c->keys()->contains("Zurich")) then {
        $x := 5 / 0;
    }
}