action main() {
    $b := media->create_board(640);
    $c := $b->create_sprite_set();
    $s1 := $b->create_ellipse(20, 20);
    $s2 := $b->create_ellipse(40, 40);
    $c->add($s1);
    $i := $c->index_of($s2);
}