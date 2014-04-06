// tries to detect when sprites are placed outside of board

//:: ExpectedOutput(backward.counterexample.found)
action main(y: Number) {
    data→board := media→create_full_board;
    $w := data→board→width;
    $h := data→board->height;

    $bg := data→board→create_rectangle(10, 10);

    //:: ExpectedOutput(assert.failed)
    $bg→set_pos(0, $y);

}


var board: Board { }
