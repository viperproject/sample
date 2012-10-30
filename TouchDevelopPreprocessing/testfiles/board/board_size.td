action main() {

    $a := math->random(100);

    $height := wall->ask_number("Give height");

    if($height > 0) then {
        $pic := media->create_picture(100,$height);
        $pic->set_pixel(math->random(100), math->random(100), colors->blue); // Error: Y might be greater than height
    } else {
        $pic := media->create_picture(100,$height); // Error: Height might be negative
        $pic->set_pixel(math->random(100), math->random(100), colors->blue); // Error: Y might be greater than height
    }

    $c := 1 / 0;

}