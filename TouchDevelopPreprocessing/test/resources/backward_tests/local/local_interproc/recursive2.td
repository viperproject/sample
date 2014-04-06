// Analysis of recursive methods should terminate and
// not lead to precision loss in "unrelated" state (boolean b here)

//:: ExpectedOutput(backward.counterexample.found)
action main(n: Number, b: Boolean) {
    data->s := 0;
    code->mult(7, 3);
    //:: ExpectedOutput(assert.failed)
    contract->assert($b, " ");
}

action mult(steps: Number, inc: Number) {
    if ($steps = 0) then {
        skip;
    } else {
        data->s := data->s + $inc;
        code->mult($steps-1, $inc);
    }

    meta private;
}

var s: Number { }