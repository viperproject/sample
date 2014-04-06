//:: ExpectedOutput(backward.counterexample.found)
action main(check: Boolean) {
     if (not $check) then {
        //:: ExpectedOutput(assert.failed)
        data->board->post_to_wall();
     } else {
        if ((data->board->is_invalid)->not) then {
           data->board->post_to_wall();
        }
     }
}

// initialized to invalid
var board : Board {

}