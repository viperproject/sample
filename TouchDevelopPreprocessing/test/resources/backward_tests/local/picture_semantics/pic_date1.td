//:: ExpectedOutput(backward.counterexample.found)
action main(verify: Boolean) {
  $p := media->choose_picture();

  if $verify then {
            if not $p->is_invalid then {
                $p->post_to_wall();
            }
  } else {
   //:: ExpectedOutput(assert.failed)
    $p->post_to_wall();
  }
}
