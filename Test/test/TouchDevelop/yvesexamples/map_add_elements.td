action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");

    $key := "myKey";
    if (web->is_connected()) then {
        $key := "myKey2";
    }

    $c->set_at($key, "myValue2");
}