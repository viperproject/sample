meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "try call";
meta rootId "A5Xupkfs2BaocncwoI7cRyx9";
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

//:: ExpectedOutput(backward.counterexample.found)
event phone_face_down() {
  //:: ExpectedOutput(assert.failed)
  media->songs->random->play;
  phone→vibrate(0.6);
  wall→set_background(colors→random);
  meta private;
}
