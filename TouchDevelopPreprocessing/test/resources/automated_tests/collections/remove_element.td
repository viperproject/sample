action main() {
    $c := collections->create_string_collection();
    $c->add("myString");
    $v := "myString";
    if web->is_connected() then {
        $v := "myString2";
    }
    $c->add($v);
    $b := $c->remove("myString");
}