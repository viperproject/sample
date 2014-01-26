action main() {
    //:: ExpectedOutput(assert.failed)
    contract->assert(false, "must always fail");
}