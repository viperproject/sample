action theOther() {

    $width := math->random(100);

    $height := wall->ask_number("Give height");

    if($height > 0) then {
        $pic := media->create_picture($width,$height);
        $pic->set_pixel(math->random($width), math->random($width), colors->blue); // Error: Y might be greater than height
        $pic->set_pixel(math->random($width), math->random($height), colors->blue);
    } else {
        $pic := media->create_picture($width,$height); // Error: Height might be negative
        $pic->set_pixel(math->random($width), math->random($height), colors->blue); // Error: Y might be negative
    }

    $c := 1 / 0;

    data→board := media→create_full_board;
    data→board→post_to_wall;
}

var board : Board {

}

action main() {

    code->theOther();

}