action main() {
   code→DoNotRunThisAction($p);
   where p() {
        //:: ExpectedOutput(unreachable)
        contract->assert(false, "should not be reachable");
    }
   code→RunThisAction($p);
   where p() {
        // does not(!) flag a unreachable warning
        "a"->post_to_wall();
    }
}

action DoNotRunThisAction( p: Action) {
}

action RunThisAction( p: Action) {
    $p->run();
}

var firstExecuted : Boolean {
  transient = true;
}