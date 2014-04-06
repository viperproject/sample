//:: ExpectedOutput(backward.counterexample.found)
action main(s: Number) {
     contract->assume($s ≥ 0);

     // what we expect
     contract->assertBackwards($s ≤ 3, "error must be in this range");

     $p := media→create_picture($s, $s);
     $x := 0;
     while ($x ≤ 3) do {
        // Imprecision introduced here because {s <= x, 0 <= x <= 3} |_| { s <= x+1, 0 <= x <= 2} |_| ...
        // cannot be expressed precisely in octagons/polyhedras

        //:: ExpectedOutput(assert.failed)
        $p→set_pixel($x, $x, colors→black);
        $x := $x + 1;
     }
}