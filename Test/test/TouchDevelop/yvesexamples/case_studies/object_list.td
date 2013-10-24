action main() {
    $msgs := collections->create_message_collection();
    $msg := social->create_message("Hello world!");
    $msgs->add($msg);

    $k := 0;
    if ($msgs->at(0)->message()->equals("Hello world!")) then {
        $k := 2;
    }

    $x := 4 / $k;
}