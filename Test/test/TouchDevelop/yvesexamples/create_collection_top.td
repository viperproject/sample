action main() {
    $c := media->songs();
    $c->random()->post_to_wall();
}