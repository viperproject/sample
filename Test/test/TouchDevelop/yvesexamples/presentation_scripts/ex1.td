action main() {
    $presidents := collections->create_string_collection();

    $presidents->add("Obama");
    if web->is_connected() then {
        $presidents->add("Cameron");
    }

    $o := $presidents->at(0);
    $o->post_to_wall();
    $c := $presidents->at(1);
    $c->post_to_wall();
}