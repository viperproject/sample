// Terminating recursion (depending on flag)

//:: ExpectedOutput(backward.counterexample.found)
action main(b: Boolean) {
    code->d($b);

    //:: ExpectedOutput(assert.failed)
    contract->assert(false, "");
}

action d(terminate: Boolean) {
    if (not $terminate) then {
        code->d($terminate);
    }

    meta private;
}