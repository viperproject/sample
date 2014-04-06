action main(input: Number) {
     $r := code→identity($input);
     contract->assert($r = $input, "always holds");
}

action identity(in: Number) returns (out: Number) {
    $out := $in;

    meta private;
}

