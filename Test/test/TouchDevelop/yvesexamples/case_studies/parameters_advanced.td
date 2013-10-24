action create(x: Number) {
    $c := collections->create_number_map();
    $c->set_at($x, $x + 1);

    $k := 0;
    if ($c->at($x) = $x+1) then {
        $k := 1;
    }

    $x := 10 / $k;
}