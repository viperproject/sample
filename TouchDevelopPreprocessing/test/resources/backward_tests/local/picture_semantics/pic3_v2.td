//:: ExpectedOutput(backward.counterexample.found)
action main(sizex: Number, sizey: Number, posx: Number, posy: Number, rotate: Boolean) {
// We only want to analyze the error for the set_pixel x coordinate.
 contract->assume($sizex > 0 and $sizey > 0 and $posx ≥ 0 and $posy ≥ 0);
 contract->assume($sizex > $posx and $sizey > $posy);

 contract->assertBackwards($rotate, "only if rotate=true we can reach error");

  $p := media→create_picture($sizex, $sizey);
  if $rotate then {
    $temp := $posx;
    $posx := $posy;
    $posy := $temp;
  }

  //:: ExpectedOutput(assert.failed)
  $p→set_pixel($posx, $posy, colors→black);
}