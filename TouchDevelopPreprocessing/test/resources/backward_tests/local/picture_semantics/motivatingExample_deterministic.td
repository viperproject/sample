//:: ExpectedOutput(backward.counterexample.found)
action main(w: Number, h: Number, radial: Boolean, rand_dir: Number, rand_x: Number, rand_y: Number) {
     contract->assume($w > 0 and $h > 0);
     contract->assume($rand_dir ≥ 0 and $rand_dir ≤ 360);
     contract->assume($rand_x ≥ 0 and $rand_x < $w);
     contract->assume($rand_y ≥ 0 and $rand_y < $w);

     $c := colors->black;
     $font_size := 0;
     $text := "foobar";

     $pic := media->create_picture($w,$h);
     $pic->post_to_wall;
     for 0 ≤ i < 3 do {
         if $radial then {
            $pic->draw_text($w/2, $h/2, $text, $font_size, $rand_dir, $c);
         } else {
            //:: ExpectedOutput(assert.failed)
            $pic->draw_text($rand_x, $rand_y, $text, $font_size, 0, $c);
         }
     }
     $pic->update_on_wall;
}