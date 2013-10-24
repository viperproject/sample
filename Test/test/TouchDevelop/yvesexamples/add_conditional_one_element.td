action main() {
    $c := collections->create_string_collection();
    if (web->is_connected()) then {
        $c->add("test");
    }

    $c->at(0)->post_to_wall();

}