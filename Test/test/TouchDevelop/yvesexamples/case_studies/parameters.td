action create(x: Number) {
    $c := collections->create_number_collection();
    $c->add($x);

    $k := 0;
    if ($c->contains($x)) then {
        $k := 1;
    }

    $x := 10 / $k;
}