meta version "v2.2,nothing";
meta name "reference tester 19";
meta icon "caution";
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
meta import json_object {
  pub "aagm"
  usage {
    action new() returns json: Json_Object
  }
  resolve core = ♻core with {
  }
}

action main() {
  ♻ json_array→new→post_to_wall;
  ♻ json_object→new→post_to_wall;
}

