// An experiment to detect when sprites are placed outside of board
// (assumptions are made about the size of the board, which may differ between devices)


//:: ExpectedOutput(backward.counterexample.found)
action main() {
    data→board := media→create_full_board;

    $w := (data→board→width / 10);
    $h := 70;
    data->x := $w / 2;
    data->y := data→board→height - 230;

    $keyLetters := "q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m" →split(",");

    foreach s in $keyLetters where true do {
        if $s→equals("a") then {
          //data->y := data->y + 80;
          data->x := 50;
        }

        $bg := data→board→create_rectangle($w - 4, $h);

        contract->assume(data->x > 0 and data->y < data->board->height);

        //:: ExpectedOutput(assert.failed)
        $bg→set_pos(data->x, data->y);

        //$fg := data→board→create_text($w, $h, 30, $s);
        //$fg→set_pos($x, $y + 12);
        // ...
     }
}


var board: Board { }

var x: Number { }
var y: Number { }