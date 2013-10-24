action main() {
    $c := collections->create_string_map();
    $c->set_at("myKey", "myValue");
    $c->set_at("myKey2", "myValue2");
    code->printMyKey($c);
}

action printMyKey(c: String_Map) {
    $c->at("myKey")->post_to_wall();
    meta page;
}