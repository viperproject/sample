action main() {
    data->c := collections->create_string_map();
    data->c->set_at("myKey", "myValue");
    data->c->set_at("myKey2", "myValue2");
}

var c : String_Map {

}