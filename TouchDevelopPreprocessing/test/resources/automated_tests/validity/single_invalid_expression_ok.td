// Tests accessing an invalid expression directly (not an invalid variable/field)

action main() {
    //:: ExpectedOutput(assert.failed)
    invalid->sprite->move(10,10);

    //:: ExpectedOutput(unreachable)
    contract->assert(false, "");
}
