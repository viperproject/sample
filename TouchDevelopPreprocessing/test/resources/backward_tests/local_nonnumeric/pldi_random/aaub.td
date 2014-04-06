// A simple, common situation: Script uses phone's features without
// first checking whether they are supported. This script crashes when no
// camera is present.

//:: ExpectedOutput(backward.counterexample.found)
action main () {
  "TouchDevelop is cool!"->post_to_wall;
  $p := senses->take_camera_picture();
  //:: ExpectedOutput(assert.failed)
  $p->post_to_wall;
  //:: ExpectedOutput(assert.failed)
  senses->record_microphone->play;
}

