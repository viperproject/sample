//:: ExpectedOutput(backward.bottom)
action main() {
     contract->assertBackwards(false, "must be unreachable");

     $x := 0;
     while ($x ≤ 10) do {
      $x := $x + 1;
     }

     //:: ExpectedOutput(assert.failed)
     contract->assert($x ≤ 11, "must hold");
}