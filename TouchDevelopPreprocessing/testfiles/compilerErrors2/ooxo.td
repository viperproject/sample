meta version "v2.2,nothing";
meta name "reference tester 9";
meta icon "caution";
meta color "#ffff7518";
//
meta platform "current";
meta import json_core {
  pub "dnoo"
  usage {
    action serialize_string(value: String) returns s: String
  }
}

action main() {
  ♻ json_core→serialize_string("")→post_to_wall;
}

