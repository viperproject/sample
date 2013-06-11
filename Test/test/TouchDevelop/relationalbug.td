action main() {
    $test := wall->ask_number("test");
    if ($test > 0) then {
        $a := math->random($test);
        contract->assert(($a - $test ≥ 1)->not,"");
	}
}
