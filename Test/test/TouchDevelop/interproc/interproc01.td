meta version "v2.2,js,ctx";
meta name "multipleReturnValuesTest";
meta platform "current";

action main() {
  $a, $c := code→go(100);
  contract→assert($a = 5, "");
  contract→assert($c→latitude = 100, "");
}

action go(c: Number) returns(a: Number, b: Location) {
  $a := 5;
  $b := locations→create_location($c, $c + 1);
  meta private;
}