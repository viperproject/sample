// The backward semantics seems to run into trouble sometimes
// and wrongly infers BOTTOM (false alarm). This is probably due to GLB calculations
// on states with different "current expressions" set somewhere...

//:: IgnoreFile(/TouchBoost/issue/42/)
action main () {
  $c := collections->create_number_collection;
  $c->add(1);
  if ($c->contains(1)) then {
    //:: ExpectedOutput(assert.failed)
    contract->assert(false, "");
  }
}
