action main(n: Number) {
     $pic := invalid->picture;

     //:: ExpectedOutput(assert.failed)
     if (math->random(5) > 3) then { contract->assert(not $pic->is_invalid, ""); }

     if ($n > 0) then {
          $pic := media->create_picture(10,10);
     }

     //:: ExpectedOutput(assert.failed)
     if (math->random(5) > 3) then { contract->assert(not $pic->is_invalid, ""); }

     code→doSomething($pic);
}

action doSomething(p: Picture) {
    //:: ExpectedOutput(assert.failed)
    if (math->random(5) > 3) then { contract->assert(not $p->is_invalid, ""); }

    $p2 := $p;

    //:: ExpectedOutput(assert.failed)
    if (math->random(5) > 3) then { contract->assert(not $p2->is_invalid, ""); }

    $p3 := $p2;

    //:: ExpectedOutput(assert.failed)
    if (math->random(5) > 3) then { contract->assert(not $p3->is_invalid, ""); }

    $p4 := $p3;

    //:: ExpectedOutput(assert.failed)
    contract->assert(not $p4->is_invalid, "");
}