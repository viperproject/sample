action main() {
    $cond1 := true;
    $cond2 := false;

    $a := 0;
    if ($cond1) then {
        $a := $a + 1;
        if ($cond2) then {
            skip;
        } else {
            $a := $a + 1;
        }
    }
    contract->assert($a = 2, "must hold");
}