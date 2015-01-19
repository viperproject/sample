action main() {
    $b := media->create_board(640);
    $c := $b->create_sprite_set();
    $s := $b->create_ellipse(20, 20);
    $s2 := $b->create_ellipse(21, 21);
    $b1 := $c->contains($s2);
    $c->add($s);
    $b2 := $c->contains($s);
}