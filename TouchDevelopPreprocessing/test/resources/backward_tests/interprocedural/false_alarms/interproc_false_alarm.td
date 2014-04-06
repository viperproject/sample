// init => pic valid (domains not able express this)

//:: ExpectedOutput(backward.bottom)
action main() {
    data->init := wall->ask_boolean("init?", "");
    if (data->init) then {
        data->pic := media->create_picture(10,5);
    }
}

event gameloop() {
    if (data->init) then {
        //:: ExpectedOutput(assert.failed)
        data->pic->update_on_wall();
    }

    meta private;
}

var init: Boolean { }

var pic: Picture { }