// rf: analysis does not reach fixpoint with octagons?

action main() {
    data->a := 1;
    data->b := 1;
    code->bla();
    code->bla2();
}

action bla() {
    data->b := 3;
    meta private;
}

action bla2() {
    data->a := data->a + 1;
    code->bla2();
    meta private;
}

var a : Number {}
var b : Number {}