// We do not really support  collections. In this test case,
// it would be nice to infer a collection containig "42".

action main () {
  $c := collections->create_number_collection;
  $n := wall->ask_number("?");
  $c->add($n);
  code->check($c);
}

//:: MissingOutput(backward.counterexample.found, /sample/issue/0000/)
action check(c: Number_Collection) {
    $b := $c->contains(42);
    if ($b) then {
      //:: ExpectedOutput(assert.failed)
      contract->assert(false, "");
    }

    meta private;
}