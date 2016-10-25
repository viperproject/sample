action main() {
    while (true) do {
      $x := records->Thing->create;
      $x->f := data→x→◈ref;
    }
}
var x : Number {
   transient = true;
}
table Thing {
  type = "Object";
  fields {
     f : Number_Ref
  }
}