action main() {
  $n := invalid→number;
  $b := true;
  contract->assert($n->is_invalid, "must hold");
  contract->assert(not $b->is_invalid, "must hold");
}