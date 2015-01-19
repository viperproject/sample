action foo(
    x:Number
){
    if $x > 10 then {
        $x->post_to_wall();
    }
}