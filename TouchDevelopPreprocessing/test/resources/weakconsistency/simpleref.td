action main() {
   data→x := 0;
   $x := data→x→◈ref;
   $x->◈set(1);
   $x->◈add(1);
   contract→assert(data→x = 2, "test");
}
var x : Number {
   transient = true;
}