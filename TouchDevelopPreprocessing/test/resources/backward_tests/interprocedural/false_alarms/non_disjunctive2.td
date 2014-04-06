
// Situation that occurs in many scripts,
// for example in "wbxsa" from the PLDI TOP scripts:
// Initialization flag is used to indicate validness of variables

// Our domains cannot represent implications like "init => global data valid",
// thus producing false alarms.


//:: ExpectedOutput(backward.bottom)
action main() {
    data->init := false;
    data->sprites := invalid->sprite_set;
    data->board := invalid->board;
}

event gameloop() {
    if (data->init) then {
        // ... drawing logic

        //:: ExpectedOutput(assert.failed)
        data->board->evolve();
    }

    meta private;
}

event shake() {
    if (not data->init) then {
        data->board := media->create_full_board();
        data->sprites := data->board->create_sprite_set();
        data->init := true;
    }

    meta private;
}

var init: Boolean { }

var sprites: Sprite_Set { }

var board: Board { }

// more global resources...