action main() {
    $n := 3;
    $x := 0;
     for 0 ≤ i < $n do {
        for 0 ≤ j < $n do {
            $x := $x + 1;
        }
     }
     contract->assert($x = $n*$n , "must hold");
}