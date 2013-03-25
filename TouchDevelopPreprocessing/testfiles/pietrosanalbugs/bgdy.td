meta version "v2.2,nothing";
meta name "quickPhrase";
//
meta platform "current";

action main() {
  code→cleanTable;
  while true do {
    while records→keptPhrases_table→count = 0 do {
      code→addPhrase;
    }
    $menu := collections→create_string_collection;
    $menu→add("Enter new phrase");
    $menu→add("Delete saved phrase");
    foreach e2 in records→keptPhrases_table where true do {
      $menu→add($e2→phrase→get);
    }
    $index := wall→pick_string("Add or remove stored phrase or pick one to copy to clipboard", "Make selection", $menu);
    if $index = 0 then {
      code→addPhrase;
    }
    else {
      if $index = 1 then {
        code→deletePhrase;
      }
      else {
        $i := 2;
        foreach e in records→keptPhrases_table where true do {
          if $i = $index then {
            $e→phrase→get→copy_to_clipboard;
            wall→clear;
            ($e→phrase→get ∥ "\nhas been copied to clipboard - switch to other app to paste it")→post_to_wall;
          }
          $i := $i + 1;
        }
      }
      skip;
    }
  }
  skip;
}
table keptPhrases {
  type = "Table";
  fields {
    phrase: String_field
  }
}

action addPhrase() {
  code→cleanTable;
  wall→clear;
  $s := wall→ask_string("Enter a phrase you wish to store");
  if not $s→is_invalid and $s→trim(" \t \n")→count > 0 then {
    $row := records→keptPhrases_table→add_row;
    $row→phrase→set($s);
  }
  meta private;
}

action deletePhrase() {
  code→cleanTable;
  $phrases := collections→create_string_collection;
  if records→keptPhrases_table→count > 0 then {
    foreach e in records→keptPhrases_table where true do {
      skip;
      $phrases→add($e→phrase→get);
    }
    wall→clear;
    $index := wall→pick_string("Choose stored phrase to delete", "", $phrases);
    $i := 0;
    foreach e2 in records→keptPhrases_table where true do {
      if $i = $index then {
        $e2→delete_row;
      }
      $i = $i + 1;
    }
  }
  meta private;
}

action cleanTable() {
  foreach e in records→keptPhrases_table where $e→phrase→get→is_invalid or $e→phrase→get→trim(" \t \n")→count < 1 do {
    $e→delete_row;
  }
  meta private;
}

