action main() {
    $c := collections->create_number_collection();
    if (web->is_connected()) then {
        $c->add(2);
    }
    $b := $c->contains(2);
}