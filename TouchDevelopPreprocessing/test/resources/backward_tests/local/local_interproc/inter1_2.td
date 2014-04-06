// Simple case: calling a method that does some stuff, but not relevant at all for backward analysis

//:: ExpectedOutput(backward.counterexample.found)
action main(n: Number) {
     code→someAction(1, true);
     if ($n = 77) then {
        //:: ExpectedOutput(assert.failed)
        contract->assert(false, "never holds");
     }
}

// Create some local variables, assign them, create some (unobservable in state) side-effects...
action someAction(a: Number, b: Boolean) {
    $c := 1;

    if ($b) then {
        $a->post_to_wall();
        $c->post_to_wall();
        $p := media->create_picture(5,5);
        $p->post_to_wall();
    }

    meta private;
}


