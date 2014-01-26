// rf: I suspect this test case has been broken for a long time.
// .. at least now it's official, and someone should fix it at some point :)

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
