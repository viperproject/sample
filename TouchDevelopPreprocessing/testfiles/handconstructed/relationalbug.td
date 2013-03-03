action main() {
    $test := wall->ask_number("test");
    if ($test > 0) then {
        $a := math->random($test);
        assert->is_false($a - $test ≥ 1);
	}
}
