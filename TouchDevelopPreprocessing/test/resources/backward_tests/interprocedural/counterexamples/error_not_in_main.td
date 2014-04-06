//:: ExpectedOutput(backward.counterexample.found)
action main(uninit: Boolean) {
    if (not $uninit) then {
        data->global := media->create_picture(10,10);
    }
    code->a();
}

action a() {
    //:: ExpectedOutput(assert.failed)
    data->global->post_to_wall();

    meta private;
}

var global: Picture { }