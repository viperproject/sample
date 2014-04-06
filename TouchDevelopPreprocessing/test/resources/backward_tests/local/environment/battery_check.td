//:: ExpectedOutput(backward.counterexample.found)
action main() {
    $level := senses->battery_level;
    $str := "Your battery level in percent: ";
    //:: ExpectedOutput(assert.failed)
    $message :=  $str ∥ $level;
    $message->post_to_wall();
}
