
action main() {
  $a := 0;
  for 0 ≤ i < 10 do {
    contract->assert($i ≤ 10,"");
    contract->assert($i ≥ 0,"");
    contract->assert($a = $i,"");
    $a := $a + 1;
  }
  contract->assert($a = 10, "");
}
