meta version "v2.2,nothing";
meta name "my script 11";
//
meta platform "current";
meta import json_core {
  pub "dnoo"
  usage {
    action new(s: String) returns json: Json_Object
  }
}

action main() {
  ♻ json_core→new("");
}

