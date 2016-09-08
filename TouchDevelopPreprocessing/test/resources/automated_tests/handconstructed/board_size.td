// rf: this test crashes with SIGSEGV in apron/libgmp.so on my system, but only with the Polka polyhedra domain

action theOther() {

    $width := math->random(100);

    $height := wall->ask_number("Give height");

    //:: ExpectedOutput(assert.failed)
    if($height > 0) then {
        $pic := media->create_picture($width,$height);
        $pic->set_pixel(math->random($width), math->random($width), colors->blue); // Error: Y might be greater than height
        $pic->set_pixel(math->random($width), math->random($height), colors->blue);
    } else {
        $pic := media->create_picture($width,$height); // Error: Height might be negative
        $pic->set_pixel(math->random($width), math->random($height), colors->blue); // Error: Y might be negative
    }

    $c := 1 / 0;

    //:: ExpectedOutput(unreachable)
    data→board := media→create_full_board;
    //:: ExpectedOutput(unreachable)
    data→board→post_to_wall;
}

var board : Board {
}

action main() {

    code->theOther();

}