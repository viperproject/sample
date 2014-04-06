// Alarm is produced in c(), but counter example inferred at entry of main.

//:: ExpectedOutput(backward.counterexample.found)
action main(start: Number, upper: Number) {
    data->global := $start;
    data->upperBound := $upper;
    code->a();
}

action a() {
    data->global := data->global + 1;
    code->b();

    meta private;
}

action b() {
    data->global := data->global + 2;
    code->c();

    meta private;
}

action c() {
    //:: ExpectedOutput(assert.failed)
    contract->assert(data->global ≤ data->upperBound, "");

    meta private;
}

var global: Number { }

var upperBound: Number { }
