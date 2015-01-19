action main() {
    $c := collections->create_number_collection();
    if web->is_connected() then {
        $c->add(3);
    } else {
        $c->add(2);
    }

    $x := $c->at(0);
    $x->post_to_wall();
}