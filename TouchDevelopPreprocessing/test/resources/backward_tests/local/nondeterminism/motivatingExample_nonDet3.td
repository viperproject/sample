//:: ExpectedOutput(backward.counterexample.found)
action main(rand_y: Number) {
     $w := wall->ask_number("Width?");
     $h := wall->ask_number("Height?");
     $radial := wall->ask_boolean("radial shape?", "");

     //contract->assume($w > 0 and $h > 0);

     $c := colors->black;
     $font_size := 0;
     $text := "foobar";

     //:: ExpectedOutput(assert.failed)
     $pic := media->create_picture($w,$h);
     $pic->post_to_wall;
     for 0 ≤ i < 3 do {
         if $radial then {
            $pic->draw_text($w/2, $h/2, $text, $font_size, math->random(360), $c);
         } else {
            //:: ExpectedOutput(assert.failed)
            $pic->draw_text(math->random($w), math->random($w), $text, $font_size, 0, $c);
         }
     }
     $pic->update_on_wall;
}