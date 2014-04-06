//:: ExpectedOutput(backward.counterexample.found)
action main() {
   $base := math->random(100);
   $offset := wall->ask_number("choose a number");
   // oops
   //:: ExpectedOutput(assert.failed)
   $result := math->sqrt($base + $offset);
}
