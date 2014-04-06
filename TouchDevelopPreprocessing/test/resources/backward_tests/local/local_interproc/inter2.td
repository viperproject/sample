//:: ExpectedOutput(backward.counterexample.found)
action main(input: Number) {
     code→someAction($input);
     if (data->n = 42) then {
        //:: ExpectedOutput(assert.failed)
        contract->assert(false, "never holds");
     }
}

action someAction(i: Number) {
    data->n := $i;
}

var n: Number { }
