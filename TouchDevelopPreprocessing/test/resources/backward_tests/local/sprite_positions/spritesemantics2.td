//:: ExpectedOutput(backward.counterexample.found)
action main(y: Number) {
    data→board := media→create_full_board;
    $w := data→board→width;
    $h := 70;

    data->x := $w / 10;
    data->y := data→board→height - 230;

    $bg := data→board→create_rectangle(10, 10);

    //:: ExpectedOutput(assert.failed)
    $bg→set_pos(0, data->y);

}


var board: Board { }

var x: Number { }
var y: Number { }