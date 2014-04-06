// Make sure we don't lose information when going backward (simplest case: called method doesn't do anything)

//:: ExpectedOutput(backward.counterexample.found)
action main(n: Number) {
     code→someAction();
     if ($n = 77) then {
        //:: ExpectedOutput(assert.failed)
        contract->assert(false, "never holds");
     }
}

action someAction() {
    skip;
    skip;
}


