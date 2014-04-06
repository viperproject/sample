meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "Test1";
meta rootId "Bvw7joz1AyYudTaAJVnnMzfB";
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

//:: ExpectedOutput(backward.counterexample.found)
event shake() {
  //:: ExpectedOutput(assert.failed)
  $s := media->songs->random->play;
  meta private;
}
