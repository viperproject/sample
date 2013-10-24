action main() {
    $c := collections->create_string_map();
    $k := "myKey";
    if (web->is_connected()) then {
        $k := "myKey2";
    }
    $c->set_at($k, "myValue");
    $c->remove("myKey");
}