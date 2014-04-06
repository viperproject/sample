//:: ExpectedOutput(backward.counterexample.found)
action main(in1: Number, in2: Number) {
    $a := code->id($in1);
    $b := code->id($in2);

    //:: ExpectedOutput(assert.failed)
    contract->assert($a ≠ $b, "");

}

action id(i: Number) returns (n: Number) {
    $n := $i;

    meta private;
}