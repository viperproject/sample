action main() {
    data->a := 1;
    data->b := 1;
    code->bla();
    code->bla2();
}

action bla() {
    data->b := 3;
}

action bla2() {
    data->a := data->a + 1;
    code->bla2();
}

var a : Number {}
var b : Number {}