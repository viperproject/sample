action main(){
    $c := collections->create_number_collection();
    $c->add(1);
    if (web->is_connected()) then {
        $c->add(2);
    }

    $c->remove_at(1);
}