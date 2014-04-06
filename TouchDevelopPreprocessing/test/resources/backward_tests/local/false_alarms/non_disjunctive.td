// init => pic valid

//:: ExpectedOutput(backward.bottom)
action main() {
    data->init := false;

    if (wall->ask_boolean("init now?", "")) then {
        data->pic := media->create_picture(10,5);
        data->init := true;
    }

    if (data->init) then {
        //:: ExpectedOutput(assert.failed)
        data->pic->post_to_wall();
    }
}

var init: Boolean { }

var pic: Picture { }