meta version "v2.2,nothing";
meta name "reference tester 15";
meta icon "caution";
meta color "#ffff7518";
//
meta platform "current";
meta import reference_tester_12 {
  pub "urgo"
  usage {
  }
  resolve json_array = ♻json_array with {
  }
  resolve core = ♻core}
meta import core {
  pub "dnoo"
  usage {
  }
}
meta import json_array {
  pub "bzjc"
  usage {
    action from_strings(strings: String_Collection) returns json: Json_Object
  }
  resolve core = ♻core with {
  }
}

action main() {
  $strings := collections→create_string_collection;
  ♻ json_array→from_strings($strings)→post_to_wall;
}

