action main() {
    $x := 0;
     for 0 ≤ i < 3 do {
         $x := $x + 1;
         contract->assert($i < 3, "must hold");
     }
     contract->assert($x = 3 , "must hold");
}