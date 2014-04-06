//:: ExpectedOutput(backward.counterexample.found)
action main(fixed: Boolean) {
    if (not $fixed) then {
        $n := wall->ask_number("any number?");
        code->calc($n);
    } else {
        code->calc(1234);
    }
}

action calc(param: Number) {
    // ...

    //:: ExpectedOutput(assert.failed)
    contract->assert($param > 0, "");

    // ...

    meta private;
}