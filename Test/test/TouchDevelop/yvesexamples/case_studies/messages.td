action main() {
    $msgs := collections->create_message_collection();
    $msg := social->create_message("text");
    $msg2 := social->create_message("text2");
    $msgs->add($msg);
    $msgs->add($msg2);

    $count := 0;
    foreach m in $msgs where $m->message->equals("text") do {
        $count := $count + 1;
    }
}