//:: ExpectedOutput(interpreter:nontermination)
action main() {
     while (true) do {
        skip;
     }
    contract->assert(false, "will never happen");
}