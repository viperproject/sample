action main () returns  {
  "TouchDevelop is cool!"->post_to_wall;
}

//:: ExpectedOutput(backward.counterexample.found)
event shake () returns  {
  //:: ExpectedOutput(assert.failed)
  $s := media->songs->random->play;

  meta private;
}

