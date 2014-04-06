//:: ExpectedOutput(backward.counterexample.found)
action main () {
  $o := senses->orientation;
  //:: ExpectedOutput(assert.failed)
  $o->post_to_wall();
}

