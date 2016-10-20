action main() {
   data→x := 0;
   data→x→◈add(1);
   contract→assert(data→x = 1, "test");
}
var x : Number {
   transient = true;
}