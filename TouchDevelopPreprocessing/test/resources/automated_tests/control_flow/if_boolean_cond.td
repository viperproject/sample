action a1(b: Boolean) {
    if ($b) then {
        // We can not prove this, as !$b is not a convex shape.
        //contract->assert($b, "holds");
    }
    else {
        contract->assert(not $b, "holds");
    }
}

action a2(n: Number) {
    if ($n ≤ 10) then {
        contract->assert($n ≤ 10, "holds");
    } else {
        contract->assert($n > 5, "holds");
    }
}