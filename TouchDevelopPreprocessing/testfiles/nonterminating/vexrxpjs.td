meta version "v2.2,nothing";
meta name "script6";
//
meta platform "current";

action main() {
  wall→set_reversed(true);
  $strings := collections→create_string_collection;
  $strings := code→loadInput;
  for 0 ≤ i < $strings→count do {
    ($i ∥ " ---- " ∥ $strings→at($i))→post_to_wall;
  }
  code→ifLoop($strings);
  code→elseLoop($strings);
  code→forloop($strings);
  code→whileloop($strings);
  "--------------------" →post_to_wall;
  data→elseArray := collections→create_number_collection;
  for 0 ≤ j < data→elseOpenArray→count do {
    $x := data→elseOpenArray→at($j);
    $x2 := data→elseCloseArray→at($j);
    while $x ≤ $x2 do {
      data→elseArray→add($x);
      $x := $x + 1;
    }
  }
  ("elseArray" ∥ data→elseArray)→post_to_wall;
  "--------------------" →post_to_wall;
  $leftLine := collections→create_number_collection;
  for 0 ≤ k < $strings→count do {
    if data→elseArray→contains($k) then {
      skip;
    }
    else {
      $leftLine→add($k);
    }
  }
  ("leftLine " ∥ $leftLine)→post_to_wall;
  "--------------------" →post_to_wall;
  data→notIfStartsAt := collections→create_number_collection;
  data→notIfEndsAt := collections→create_number_collection;
  for 0 ≤ l < data→ifOpenArray→count do {
    data→notIfStartsAt→add(data→ifOpenArray→at($l));
    data→notIfEndsAt→add(data→ifCloseArray→at($l) + 1);
  }
  ("notIfStartsAt" ∥ data→notIfStartsAt)→post_to_wall;
  ("notIfEndsAt" ∥ data→notIfEndsAt)→post_to_wall;
  data→elseStartsAt := collections→create_number_collection;
  data→elseEndsAt := collections→create_number_collection;
  for 0 ≤ m < data→elseOpenArray→count do {
    data→elseStartsAt→add(data→elseOpenArray→at($m));
    data→elseEndsAt→add(data→elseCloseArray→at($m) + 1);
  }
  ("elseStartsAt" ∥ data→elseStartsAt)→post_to_wall;
  ("elseEndsAt" ∥ data→elseEndsAt)→post_to_wall;
  "--------------------" →post_to_wall;
  for 0 ≤ n < data→notIfStartsAt→count do {
    if data→elseArray→contains(data→notIfStartsAt→at($n)) then {
      data→notIfStartsAt→set_at($n, 0);
      data→notIfEndsAt→set_at($n, 0);
    }
  }
  ("notIfStartsAt" ∥ data→notIfStartsAt)→post_to_wall;
  ("notIfEndsAt" ∥ data→notIfEndsAt)→post_to_wall;
  "--------------------" →post_to_wall;
  $x3 := 0;
  $count := 0;
  for 0 ≤ i2 < data→elseStartsAt→count do {
    $x3 := data→elseStartsAt→at($i2);
    $count := 0;
    for 0 ≤ i3 < data→elseArray→count do {
      if $x3 = data→elseArray→at($i3) then {
        $count := $count + 1;
      }
    }
    if $count > 1 then {
      data→elseStartsAt→set_at($i2, 0);
      data→elseEndsAt→set_at($i2, 0);
    }
  }
  ("elseStartsAt" ∥ data→elseStartsAt)→post_to_wall;
  ("elseEndsAt" ∥ data→elseEndsAt)→post_to_wall;
  "--------------------" →post_to_wall;
  for 0 ≤ n1 < data→forOpenArray→count do {
    if data→elseArray→contains(data→forOpenArray→at($n1)) then {
      data→forOpenArray→set_at($n1, 0);
      data→forCloseArray→set_at($n1, 0);
    }
  }
  ("forClosesAt " ∥ data→forCloseArray)→post_to_wall;
  ("forOpensAt " ∥ data→forOpenArray)→post_to_wall;
  "--------------------" →post_to_wall;
  for 0 ≤ n2 < data→whileOpenArray→count do {
    if data→elseArray→contains(data→whileOpenArray→at($n2)) then {
      data→whileOpenArray→set_at($n2, 0);
      data→whileCloseArray→set_at($n2, 0);
    }
  }
  ("whileClosesAt " ∥ data→whileCloseArray)→post_to_wall;
  ("whileOpensAt " ∥ data→whileOpenArray)→post_to_wall;
}

action loadInput() returns strings2: String_Collection {
  $strings := collections→create_string_collection;
  $strings→add("action main() {");
  $strings→add("wall→set_reversed(true);");
  $strings→add("$x := 10;");
  $strings→add(" $y := 20;");
  $strings→add(" $z := 30;");
  $strings→add(" if $x = 10 then {");
  $strings→add("for 0 ≤ i < 3 do {");
  $strings→add("$i→post_to_wall;");
  $strings→add("}");
  $strings→add("if $y = 20 then {");
  $strings→add(" if $z = 30 then {");
  $strings→add("for 0 ≤ i < 3 do {");
  $strings→add("$i→post_to_wall;");
  $strings→add("while $x < 3 do {");
  $strings→add("$x := $x - 1;");
  $strings→add("}");
  $strings→add("}");
  $strings→add("}");
  $strings→add(" else {");
  $strings→add("\"z is not 30\" →post_to_wall;");
  $strings→add("for 0 ≤ i < 3 do {");
  $strings→add("$i→post_to_wall;");
  $strings→add("while $x < 3 do {");
  $strings→add("$x := $x - 1;");
  $strings→add(" }");
  $strings→add(" }");
  $strings→add(" }");
  $strings→add(" $z→post_to_wall;");
  $strings→add("}");
  $strings→add(" else {");
  $strings→add(" if $z = 30 then {");
  $strings→add(" skip;");
  $strings→add("}");
  $strings→add(" else {");
  $strings→add("\"z is not 30\" →post_to_wall;");
  $strings→add(" }");
  $strings→add("\"y is not 20\" →post_to_wall;");
  $strings→add(" }");
  $strings→add(" $y→post_to_wall;");
  $strings→add("}");
  $strings→add(" $x→post_to_wall;");
  $strings→add("}");
  skip;
  $strings2 := $strings;
  meta private;
}

action ifLoop(strings2: String_Collection) {
  $strings := collections→create_string_collection;
  $strings := $strings2;
  skip;
  data→ifOpenArray := collections→create_number_collection;
  data→ifOpenArray→add(0);
  data→ifCloseArray := collections→create_number_collection;
  data→ifCloseArray→add(0);
  $openBraceArray := collections→create_number_collection;
  $openBraceArray→add(0);
  $closeBraceArray := collections→create_number_collection;
  $closeBraceArray→add(0);
  $ptr := 0;
  $bool := false;
  $ifStarts := false;
  $ifOpenCount := 0;
  $ifCloseCount := 0;
  for 0 ≤ i < $strings→count do {
    if $strings→at($i)→contains("if") then {
      $ifStarts := true;
      $ifOpenCount := $ifOpenCount + 1;
      $ptr := $ifOpenCount;
      data→ifOpenArray→add(0);
      data→ifCloseArray→add(0);
      $openBraceArray→add(0);
      $closeBraceArray→add(0);
      data→ifOpenArray→set_at($ptr, $i);
      data→ifCloseArray→set_at($ptr, 0);
      $openBraceArray→set_at($ptr, 0);
      $closeBraceArray→set_at($ptr, 0);
    }
    if $ifStarts→equals(true) and $strings→at($i)→contains("{") then {
      $openBraceArray→set_at($ptr, $openBraceArray→at($ptr) + 1);
    }
    if $ifStarts→equals(true) and $strings→at($i)→contains("}") then {
      $closeBraceArray→set_at($ptr, $closeBraceArray→at($ptr) + 1);
    }
    if $ifStarts→equals(true) and $openBraceArray→at($ptr) = $closeBraceArray→at($ptr) then {
      data→ifCloseArray→set_at($ptr, $i);
      $ifCloseCount := $ifCloseCount + 1;
      $bool := false;
      while $bool→equals(false) do {
        if data→ifCloseArray→at($ptr) = 0 then {
          $bool := true;
        }
        else {
          $ptr := $ptr - 1;
        }
      }
      if $ifCloseCount = $ifOpenCount then {
        $ifStarts := false;
      }
    }
  }
  ("ifloop opens at : " ∥ data→ifOpenArray)→post_to_wall;
  ("ifloop closes at : " ∥ data→ifCloseArray)→post_to_wall;
}

action elseLoop(strings2: String_Collection) {
  $strings := collections→create_string_collection;
  $strings := $strings2;
  skip;
  data→elseOpenArray := collections→create_number_collection;
  data→elseOpenArray→add(0);
  data→elseCloseArray := collections→create_number_collection;
  data→elseCloseArray→add(0);
  $openBraceArray := collections→create_number_collection;
  $openBraceArray→add(0);
  $closeBraceArray := collections→create_number_collection;
  $closeBraceArray→add(0);
  $ptr := 0;
  $bool := false;
  $elseStarts := false;
  $elseOpenCount := 0;
  $elseCloseCount := 0;
  for 0 ≤ i < $strings→count do {
    if $strings→at($i)→contains("else") then {
      $elseStarts := true;
      $elseOpenCount := $elseOpenCount + 1;
      $ptr := $elseOpenCount;
      data→elseOpenArray→add(0);
      data→elseCloseArray→add(0);
      $openBraceArray→add(0);
      $closeBraceArray→add(0);
      data→elseOpenArray→set_at($ptr, $i);
      data→elseCloseArray→set_at($ptr, 0);
      $openBraceArray→set_at($ptr, 0);
      $closeBraceArray→set_at($ptr, 0);
    }
    if $elseStarts→equals(true) and $strings→at($i)→contains("{") then {
      $openBraceArray→set_at($ptr, $openBraceArray→at($ptr) + 1);
    }
    if $elseStarts→equals(true) and $strings→at($i)→contains("}") then {
      $closeBraceArray→set_at($ptr, $closeBraceArray→at($ptr) + 1);
    }
    if $elseStarts→equals(true) and $openBraceArray→at($ptr) = $closeBraceArray→at($ptr) then {
      data→elseCloseArray→set_at($ptr, $i);
      $elseCloseCount := $elseCloseCount + 1;
      $bool := false;
      while $bool→equals(false) do {
        if data→elseCloseArray→at($ptr) = 0 then {
          $bool := true;
        }
        else {
          $ptr := $ptr - 1;
        }
      }
      if $elseCloseCount = $elseOpenCount then {
        $elseStarts := false;
      }
    }
  }
  ("elseloop opens at : " ∥ data→elseOpenArray)→post_to_wall;
  ("elseloop closes at : " ∥ data→elseCloseArray)→post_to_wall;
  meta private;
}
var ifOpenArray : Number_Collection {
}
var ifCloseArray : Number_Collection {
}
var elseOpenArray : Number_Collection {
}
var elseCloseArray : Number_Collection {
}
var elseArray : Number_Collection {
}
var notIfStartsAt : Number_Collection {
}
var notIfEndsAt : Number_Collection {
}
var elseStartsAt : Number_Collection {
}
var elseEndsAt : Number_Collection {
}

action forloop(strings2: String_Collection) {
  $strings := collections→create_string_collection;
  $strings := $strings2;
  skip;
  data→forOpenArray := collections→create_number_collection;
  data→forOpenArray→add(0);
  data→forCloseArray := collections→create_number_collection;
  data→forCloseArray→add(0);
  $openBraceArray := collections→create_number_collection;
  $openBraceArray→add(0);
  $closeBraceArray := collections→create_number_collection;
  $closeBraceArray→add(0);
  $ptr := 0;
  $bool := false;
  $forStarts := false;
  $forOpenCount := 0;
  $forCloseCount := 0;
  for 0 ≤ i < $strings→count do {
    if $strings→at($i)→contains("for") then {
      $forStarts := true;
      $forOpenCount := $forOpenCount + 1;
      $ptr := $forOpenCount;
      data→forOpenArray→add(0);
      data→forCloseArray→add(0);
      $openBraceArray→add(0);
      $closeBraceArray→add(0);
      data→forOpenArray→set_at($ptr, $i);
      data→forCloseArray→set_at($ptr, 0);
      $openBraceArray→set_at($ptr, 0);
      $closeBraceArray→set_at($ptr, 0);
    }
    if $forStarts→equals(true) and $strings→at($i)→contains("{") then {
      $openBraceArray→set_at($ptr, $openBraceArray→at($ptr) + 1);
    }
    if $forStarts→equals(true) and $strings→at($i)→contains("}") then {
      $closeBraceArray→set_at($ptr, $closeBraceArray→at($ptr) + 1);
    }
    if $forStarts→equals(true) and $openBraceArray→at($ptr) = $closeBraceArray→at($ptr) then {
      data→forCloseArray→set_at($ptr, $i);
      $forCloseCount := $forCloseCount + 1;
      $bool := false;
      while $bool→equals(false) do {
        if data→forCloseArray→at($ptr) = 0 then {
          $bool := true;
        }
        else {
          $ptr := $ptr - 1;
        }
      }
      if $forCloseCount = $forOpenCount then {
        $forStarts := false;
      }
    }
  }
  ("forloop opens at : " ∥ data→forOpenArray)→post_to_wall;
  ("forloop closes at : " ∥ data→forCloseArray)→post_to_wall;
  meta private;
}

action whileloop(strings2: String_Collection) {
  $strings := collections→create_string_collection;
  $strings := $strings2;
  skip;
  data→whileOpenArray := collections→create_number_collection;
  data→whileOpenArray→add(0);
  data→whileCloseArray := collections→create_number_collection;
  data→whileCloseArray→add(0);
  $openBraceArray := collections→create_number_collection;
  $openBraceArray→add(0);
  $closeBraceArray := collections→create_number_collection;
  $closeBraceArray→add(0);
  $ptr := 0;
  $bool := false;
  $whileStarts := false;
  $whileOpenCount := 0;
  $whileCloseCount := 0;
  skip;
  for 0 ≤ i < $strings→count do {
    if $strings→at($i)→contains("while") then {
      $whileStarts := true;
      $whileOpenCount := $whileOpenCount + 1;
      $ptr := $whileOpenCount;
      data→whileOpenArray→add(0);
      data→whileCloseArray→add(0);
      $openBraceArray→add(0);
      $closeBraceArray→add(0);
      data→whileOpenArray→set_at($ptr, $i);
      data→whileCloseArray→set_at($ptr, 0);
      $openBraceArray→set_at($ptr, 0);
      $closeBraceArray→set_at($ptr, 0);
    }
    if $whileStarts→equals(true) and $strings→at($i)→contains("{") then {
      $openBraceArray→set_at($ptr, $openBraceArray→at($ptr) + 1);
    }
    if $whileStarts→equals(true) and $strings→at($i)→contains("}") then {
      $closeBraceArray→set_at($ptr, $closeBraceArray→at($ptr) + 1);
    }
    if $whileStarts→equals(true) and $openBraceArray→at($ptr) = $closeBraceArray→at($ptr) then {
      data→whileCloseArray→set_at($ptr, $i);
      $whileCloseCount := $whileCloseCount + 1;
      $bool := false;
      while $bool→equals(false) do {
        if data→whileCloseArray→at($ptr) = 0 then {
          $bool := true;
        }
        else {
          $ptr := $ptr - 1;
        }
      }
      if $whileCloseCount = $whileOpenCount then {
        $whileStarts := false;
      }
    }
  }
  ("whileloop starts at : " ∥ data→whileOpenArray)→post_to_wall;
  ("whileloop ends at : " ∥ data→whileCloseArray)→post_to_wall;
  meta private;
}
var forOpenArray : Number_Collection {
}
var forCloseArray : Number_Collection {
}
var whileOpenArray : Number_Collection {
}
var whileCloseArray : Number_Collection {
}
