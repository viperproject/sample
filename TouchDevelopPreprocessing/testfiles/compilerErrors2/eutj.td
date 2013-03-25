meta version "v2.2,nothing";
meta name "reference tester 11";
meta icon "caution";
meta color "#ffff7518";
//
meta platform "current";
meta import json_array {
  pub "bzjc"
  usage {
  }
  resolve core = ♻core with {
  }
}
meta import core {
  pub "dnoo"
  usage {
    action serialize_string(value: String) returns s: String
  }
}

action main() {
  ♻ core→serialize_string("")→post_to_wall;
}

