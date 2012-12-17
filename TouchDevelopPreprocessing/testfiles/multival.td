action main() {
    $v,$z := code->multival(3);
}

action multival(i: Number) returns x: Number, y: Number {
    $x := $i;
    $y := $i+1;
}