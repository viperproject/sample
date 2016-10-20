action main() {
   // === 4 times: Read stringGlobal
   $x := data→stringGlobal;
   data→stringGlobal→post_to_wall;
   wall→prompt(data→stringGlobal);
   $sGref := data→stringGlobal→◈ref;
   $sGref→◈ref→◈ref→◈ref→◈get→post_to_wall;
   // === 4 times: set / test and set / clear stringGlobal
   data→stringGlobal := "dfsdfsdf";
   $sGref→◈ref→◈test_and_set("99");
   $sGref→◈set("88");
   $sGref→◈clear;
   // === 3 times: restricted reads of value
   $sGref→◈get→starts_with("blublablu");
   data→stringGlobal→is_invalid;
   data→stringGlobal→count;
   // === 3 Times, change numerical value
   $nGref := data→numberGlobal→◈ref;
   $nGref→◈add(1);
   $nGref→◈clear;
   data→numberGlobal→◈set(0);
   // === 2 Times, restricted reads of ref3
   $nGref→◈get < 5;
   5 < $nGref→◈get;
}
var numberGlobal : Number {
  cloudenabled = true;
}
var stringGlobal : String {
  cloudenabled = true;
}