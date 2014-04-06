//:: ExpectedOutput(backward.counterexample.found)
action main() {
     $w := wall->ask_number("Width?");
     $h := wall->ask_number("Height?");
     $radial := wall->ask_boolean("radial shape?", "");

     contract->assume($w > 0 and $h > 0);

     $c := colors->black;
     $font_size := 0;
     $text := "foobar";

     $pic := media->create_picture($w,$h);
     $pic->post_to_wall;
        code->paint($pic, $radial);
     $pic->update_on_wall;
}

action paint(pic: Picture, rad: Boolean) {
     $c := colors->black;
     $font_size := 0;
     $text := "foobar";
     $w := $pic->width;
     $h := $pic->height;

              if $rad then {
                 $pic->draw_text($w/2, $h/2, $text, $font_size, math->random(360), $c);
              } else {
                 //:: ExpectedOutput(assert.failed)
                 $pic->draw_text(math->random($w), math->random($w), $text, $font_size, 0, $c);
              }

     meta private;
}