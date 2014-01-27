// Tests accessing an invalid expression directly (not an invalid variable/field)

action main() {
    //:: ExpectedOutput(assert.failed)
    invalid->sprite->move(10,10);

    // Curiously, this no alarm is triggered here, because we get bottom after the error above
    contract->assert(false, "");
}
