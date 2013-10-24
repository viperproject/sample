action main() {
    data->s := collections->create_string_map();
    data->s->set_at("myKey", "myValue");
    data->s->set_at("myKey2", "myValue2");
}

var s : String_Map {
}