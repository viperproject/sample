action main() {
    $b := media->create_board(640);
    $c := $b->create_sprite_set();
    $c->remove_first();
    $s := $b->create_ellipse(20, 20);
    $c->add($s);
    $c->remove_first();
}