action main () returns  {
  "Brian is cool! Now dance to the music! ðŸŽ§"->post_to_wall;
}

//:: ExpectedOutput(backward.counterexample.found)
event shake () returns  {
  //:: ExpectedOutput(assert.failed)
  $s := media->songs->random->play;

  meta private;
}

action go () returns  {
  skip;

  meta private;
}