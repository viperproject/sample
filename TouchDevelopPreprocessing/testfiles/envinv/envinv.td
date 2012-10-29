action main() {

    $a := wall->ask_number(""); // returns some valid number
    $x := invalid->number; // returns an invalid number

    $y := 5 + $x; // This should be an error
    $b := $a + 1; // This should not be an error 

    if ($a->is_invalid) then {
        $a := 5;
    }

    $b := $a + 1; // This should work

    $c := senses->motion; // This should be an error

    if (senses->has_motion) then {
        $c := senses->motion; // This should work
    }

}