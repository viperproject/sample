meta version "v2.2,nothing";
meta name "script5";
//
meta platform "current";

action main() {
  wall→set_reversed(true);
  $strings := collections→create_string_collection;
  $strings→add("action main() {");
  $strings→add("wall→set_reversed(true);");
  $strings→add(" \"Hello world!\" →post_to_wall;");
  $strings→add(" $x := 0;");
  $strings→add("data→a := 3;");
  $strings→add(" data→b := data→a + 3;");
  $strings→add("data→c := data→b * data→b;");
  $strings→add(" for 0 ≤ i < 3 do {");
  $strings→add(" $i→post_to_wall;");
  $strings→add(" if data→a > data→b then {");
  $strings→add("$x := data→a;");
  $strings→add("}");
  $strings→add(" else {");
  $strings→add("if data→b > data→a then {");
  $strings→add(" $x := data→b;");
  $strings→add("}");
  $strings→add("}");
  $strings→add("if data→c > $x then {");
  $strings→add(" $x := data→c;");
  $strings→add("    }");
  $strings→add("($x ∥ \" is greater of all\")→post_to_wall;");
  $strings→add("}");
  $strings→add("}");
  $strings→add("var a : Number {");
  $strings→add("}");
  $strings→add("var b : Number {");
  $strings→add("}");
  $strings→add("var c : Number {");
  $strings→add("}");
  for 0 ≤ i < $strings→count do {
    ($i ∥ "----" ∥ $strings→at($i))→post_to_wall;
  }
  $forStarts := false;
  $forOpenbraces := 0;
  $forClosebraces := 0;
  $forStartsAt := 0;
  $forEndsAt := 0;
  for 0 ≤ i1 < $strings→count do {
    if $strings→at($i1)→contains("for") then {
      $forStarts := true;
      $forStartsAt := $i1;
    }
    if $forStarts→equals(true) and $strings→at($i1)→contains("{") then {
      $forOpenbraces := $forOpenbraces + 1;
    }
    if $forStarts→equals(true) and $strings→at($i1)→contains("}") then {
      $forClosebraces := $forClosebraces + 1;
    }
    if $forStarts→equals(true) and $forOpenbraces = $forClosebraces then {
      $forEndsAt := $i1;
      $forStarts := false;
    }
    ($i1 ∥ $forOpenbraces ∥ $forClosebraces)→post_to_wall;
  }
  ("forStartsAt = " ∥ $forStartsAt)→post_to_wall;
  ("forEndsAt = " ∥ $forEndsAt)→post_to_wall;
}

