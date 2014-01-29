action main(n: Number) {
     $pic := invalid->picture;
     if ($n > 0) then {
          $pic := media->create_picture(10,10);
     }
     code→doSomething($pic);
}

action doSomething(p: Picture) {
    $p2 := $p;
    $p3 := $p2;
    $p4 := $p3;

    //:: ExpectedOutput(assert.failed)
    contract->assert(not $p4->is_invalid, "");
}