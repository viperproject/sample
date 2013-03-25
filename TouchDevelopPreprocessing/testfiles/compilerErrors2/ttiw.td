meta version "v2.2,nothing";
meta name "lib test";
//
meta seed "<seed xmlns=\"http://research.microsoft.com/revisionservice\">\r\n  <metaschema>1.1</metaschema>\r\n  <domain uid=\"0.0\" def=\"D0-sS\" />\r\n  <property uid=\"0.1\" name=\"\" dom=\"0.0\" cod=\"CS\" />\r\n  <entries property=\"0.1\" />\r\n</seed>";
meta platform "current";
meta import json_array_test6 {
  pub "miph"
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
  $json := ♻ json_array_test6→new();
}

