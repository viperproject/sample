
action main() {
  $a := 5;
  $b := 6;
  contract->assert($a + $b = 11, "simple addition should work");
}
