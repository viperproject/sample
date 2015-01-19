action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");

    $k := "myKey";
    if web->is_connected() then {
        $k := "myKey2";
    }

    $c->set_at($k, "myValue2");

    $c->remove("myKey");
}