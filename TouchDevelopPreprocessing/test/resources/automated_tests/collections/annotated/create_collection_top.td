action main() {
    $c := media->songs();
    //:: ExpectedOutput(assert.failed)
    $c->random()->post_to_wall();
}