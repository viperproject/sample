
action main() {
  $b := 4;
  $a := ($b > 5);
  $c := ($b > 3);
  contract->assert((not $a) and $c, "");
}
