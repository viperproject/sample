action main() {
    //:: ExpectedOutput(assert.failed)
    contract->assert(false, "doesnt hold");
}