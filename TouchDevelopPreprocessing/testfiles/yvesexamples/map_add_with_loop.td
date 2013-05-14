action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");
    for 0 ≤ i < 2 do {
        $k := "myKey"->concat($i->to_string());
        $v := "myValue"->concat($i->to_string());
        $c->set_at($k, $v);
    }
}