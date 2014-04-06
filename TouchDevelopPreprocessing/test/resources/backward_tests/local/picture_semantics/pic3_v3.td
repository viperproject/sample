action main(sizex: Number, sizey: Number, posx: Number, posy: Number, rotate: Boolean) {
    if ($sizex > 0 and
        $sizey > 0 and
        $posx ≥ 0 and
        $posy ≥ 0 and
        $sizex > $posx and
        $sizey > $posy) then {

        $p := media→create_picture($sizex, $sizey);

        code->paint($p, $posx, $posy, $rotate);
    }
}

//:: ExpectedOutput(backward.counterexample.found)
action paint(p: Picture, posx: Number, posy: Number, rotate: Boolean) {


       $in := wall->ask_number("foo?");

       if $rotate and $posy < 10 and $in = 77 then {
         $temp := $posx;
         $posx := $posy;
         $posy := $temp;
       }

       //:: ExpectedOutput(assert.failed)
       $p→set_pixel($posx, $posy, colors→black);

       meta private;
 }