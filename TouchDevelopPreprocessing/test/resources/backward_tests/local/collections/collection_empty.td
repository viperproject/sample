//:: ExpectedOutput(backward.counterexample.found)
action main () {
  $c := media->songs;
  $s := $c->random();
  //:: ExpectedOutput(assert.failed)
  contract->assert(not $s->is_invalid(), "");
}