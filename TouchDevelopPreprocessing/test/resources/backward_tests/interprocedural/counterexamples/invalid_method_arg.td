//:: ExpectedOutput(backward.counterexample.found)
action main() {
     $pic := invalid->picture;
     code→drawSomething($pic);
}

action drawSomething(p: Picture) {
    //:: ExpectedOutput(assert.failed)
    $p->set_pixel(2,3, colors->black);

    meta private;
}


