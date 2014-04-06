//:: ExpectedOutput(backward.counterexample.found)
action main() {
     $flag := wall->ask_boolean("flag?", "");

     if $flag then {
        skip;
     } else {
        //:: ExpectedOutput(assert.failed)
        contract->assert(false, "blub");
     }
}