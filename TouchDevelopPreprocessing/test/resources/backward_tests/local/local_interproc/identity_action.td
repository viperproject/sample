//:: ExpectedOutput(backward.counterexample.found)
action main(input: Number) {
     $r := code→identity($input);
     //:: ExpectedOutput(assert.failed)
     contract->assert($r ≠ 42, "");
}

action identity(in: Number) returns (out: Number) {
    $out := $in;
}

