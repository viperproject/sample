meta version "v2.2,nothing";
meta name "reference tester 12";
meta icon "caution";
meta color "#ffff7518";
//
meta isLibrary "yes";
meta platform "current";
meta import json_array {
  pub "bzjc"
  usage {
    action new() returns json: Json_Object
  }
  resolve core = ♻core with {
  }
}
meta import core {
  pub "dnoo"
  usage {
  }
}

action main() {
  ♻ json_array→new→post_to_wall;
}

