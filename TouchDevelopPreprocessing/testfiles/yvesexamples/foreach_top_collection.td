action main() {
    $c := media->songs();
    foreach e in $c where true do{
        $e->post_to_wall();
    }
}