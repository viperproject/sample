// Explanation: error in private action 'sub' is only reachable from 'foo' public action.
// Our interprocedural analysis looks at the "roots" of the call graph and tries to infer
// counterexamples from there.

action main() {
    data->board := media->create_full_board();
    code->sub();
}

//:: ExpectedOutput(backward.counterexample.found)
action foo() {
    // board not initialized
    code->sub();
}

action sub() {
    //:: ExpectedOutput(assert.failed)
    data->board->evolve();
    meta private;
}

var board: Board { }
