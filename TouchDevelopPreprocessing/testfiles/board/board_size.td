action main() {

    $a := math->random(100);

    $height := wall->ask_number("Give height");

    if($height > 0) then {
        $pic := media->create_picture(100,$height);
        $pic->set_pixel(math->random(100), math->random(100), colors->blue);
    } else {
        $pic := media->create_picture(100,$height);
        $pic->set_pixel(math->random(100), math->random(100), colors->blue);
    }

    $c := 1 / 0;

}