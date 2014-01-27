action main() {
    data->a := 0;
    code->go();
}

action go() {
    data->a :=  data->a + 1;
    code->go();

    meta private;
}

var a : Number {}
