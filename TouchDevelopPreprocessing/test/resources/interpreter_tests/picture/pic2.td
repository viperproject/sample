action main(b: Boolean) {
  $p := invalid→picture;
  if $b then {
    $p := media→create_picture(10, 10);
  }
  else {
    $p := media→create_picture(5, 10);
  }
  $w := $p->width;
  contract->assert($w ≥ 0, "can prove that");

  //:: ExpectedOutput(assert.failed)
  contract->assert($p->width ≥ 7, "does not always hold");

}
