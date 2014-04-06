//:: ExpectedOutput(backward.counterexample.found)
action main(s: Number, xs: Number) {
  contract->assume($s > 0);

  contract->assertBackwards($xs ≤ 0, "this must hold for the strange resize behavior to occur");

  $p := media→create_picture($s, $s);
  $p->resize($xs, $s);

  //:: ExpectedOutput(assert.failed)
  contract->assert($xs ≥ $p->width, "seems like a reasonable assumption but doesn't hold");
}