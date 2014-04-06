//:: ExpectedOutput(backward.bottom)
action main(x: Number) returns (y: Number) {
     // must not be reachable from end (false alarm)
     contract->assertBackwards(false, "must not trigger alarm");

     if ($x > 0) then {
        $y := $x;
     } else {
        $y := -$x;
     }

     if ($y > 5) then {
         //:: ExpectedOutput(assert.failed)
        contract->assert($x < -5 or 5 < $x, "must hold");
     }
}