action main() {
     $x := 0;
     $y := 1;

     while ($x ≤ 10) do {
        $x := $x + 1;
        $y := $y + 1;
     }

    contract->assert($x = 11, "must hold");
    contract->assert($y = 12, "must hold");
}