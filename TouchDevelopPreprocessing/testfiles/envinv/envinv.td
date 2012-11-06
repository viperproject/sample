action main() {

    $a := wall->ask_number("");
    $x := invalid->number;

    $y := 5 + $x; // Error: x might be invalid
    $b := $a + 1;

    if ($a->is_invalid) then {
        $a := 5;
    }

    $b := $a + 1;

    $c := senses->motion; // Error: might not have the correct capabilities for this

    if (senses->has_motion) then {
        $c := senses->motion; // This should work
    }

}