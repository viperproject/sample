meta version "v2.2,nothing";
meta name "lib with types";
//
meta isLibrary "yes";
meta platform "current";

action mk() returns t1: t {
  $t1 := records→t→create;
}
table t {
  type = "Object";
  fields {
    f: String_field
  }
}

action get_f(t1: t) returns s: String {
  $s := $t1→f→get;
}

action set_f(t1: t, s: String) {
  $t1→f→set($s);
}

