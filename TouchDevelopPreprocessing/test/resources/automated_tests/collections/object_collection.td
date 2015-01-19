action main() {
    $m := social->create_message("foo");
    $c := collections->create_message_collection();
    $c->add($m);
}