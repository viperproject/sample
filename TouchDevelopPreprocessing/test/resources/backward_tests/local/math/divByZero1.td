//:: ExpectedOutput(backward.counterexample.found)
action main(a: Number, c: Number) {
     $b := 7;
     // a,b,c are inputs where b is initialized to 7

     // after the backwards analysis, we must know at this point that the input state leading to a state with c = 0
     // is: {a = 1, b = 7, c = 0}
     contract->assertBackwards($a = 1 and $b = 7 and $c = 0, "must hold");

     if ($a = 1) then {
        //:: ExpectedOutput(assert.failed)
        $r := 1 / $c;
     }
}