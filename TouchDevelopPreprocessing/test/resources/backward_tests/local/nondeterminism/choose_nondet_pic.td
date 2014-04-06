
//:: ExpectedOutput(backward.counterexample.found)
action card_call() {
  for 0 ≤ i < 10 do {
    $p := media->choose_picture();
    if (not $p->is_invalid) then {
        //:: ExpectedOutput(assert.failed)
        contract->assert($p->width ≠ 10, "");
    }
  }
}
