action main() {
    foreach s in media->songs() where true do {
        $s->post_to_wall();
    }
}