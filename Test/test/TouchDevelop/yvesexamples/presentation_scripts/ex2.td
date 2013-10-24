action print_presidents(presidents:String_Map) {
    if $presidents->keys->contains("USA") then {
        $p := $presidents->at("USA");
        $p->post_to_wall();
    } else {
        "unknown"->post_to_wall();
    }
}