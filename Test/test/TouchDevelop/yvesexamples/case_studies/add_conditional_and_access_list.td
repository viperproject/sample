action main() {
    $s := collections->create_string_collection();

    if (wall->ask_boolean("Choose", "Zurich, Bern")) then {
        $s->add("Zurich");
    }
    else {
        $s->add("Bern");
    }

    $c := $s->at(0);

    $c->post_to_wall();
}