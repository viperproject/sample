action main(n: Number, b: Boolean) {
    contract->assert($n = 0 and ($b->not), "must hold");
}