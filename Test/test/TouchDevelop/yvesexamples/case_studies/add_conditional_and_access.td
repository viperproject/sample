action main() {
    $s := collections->create_string_map();

    if (wall->ask_boolean("Choose", "Zurich, Bern")) then {
        $s->set_at("Switzerland", "Zurich");
    }
    else {
        $s->set_at("Switzerland", "Bern");
    }

    $c := $s->at("Switzerland");

    $c->post_to_wall();
}