// Test heap concretization where "pic -> {PP1,PP2}"
// (concretization picks one possible assignment of pic)

action main(choice: Boolean) {
    if ($choice) then {
        data->pic := media->create_picture(5, 5);  // PP1
    } else {
        data->pic := media->create_picture(10, 10); // PP2
    }
}

//:: ExpectedOutput(backward.counterexample.found)
event gameloop() {
//:: ExpectedOutput(assert.failed)
    data->pic->set_pixel(20, 20, colors->black());
}

var pic: Picture { }