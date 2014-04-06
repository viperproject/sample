/*
 * Test calling context dependent non-determinism sources (instead of a single
 * summary id, create ids for every possible calling point)
 *
 * This testcase only succeeds with a context sensitive analysis.
 * Don't want to enable this by default, so it's ignored here.
 */

//:: IgnoreFile(/TouchBoost/issue/0/)

//:: ExpectedOutput(backward.counterexample.found)
action main() {
    $a := code->ask_num();
    $b := code->ask_num() + 42;

    //:: ExpectedOutput(assert.failed)
    contract->assert($a ≠ $b, "");

}

action ask_num() returns (n: Number) {
    $n := wall->ask_number("which number?");

    meta private;
}