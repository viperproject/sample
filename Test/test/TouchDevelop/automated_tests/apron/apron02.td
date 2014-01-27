
action main() {
  $a := 5;
  $b := 0;
  if (math->random(2) > 0) then {
    $b := 3;
  } else {
    $b := 4;
  }
  contract->assert($a + $b ≤ 9, "less than 9");
  contract->assert($a + $b ≥ 8, "greater 8");
}
