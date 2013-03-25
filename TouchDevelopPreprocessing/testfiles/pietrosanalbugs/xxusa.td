meta version "v2.2,nothing";
meta name "Spelling List";
meta icon "bulletlist";
meta color "#ff85bb65";
// Helper app for learning a list of spellings. There are two modes: one for entering, reviewing or deleting the words; the other mode is a quiz where the stored words are asked at random.
meta platform "current";

action main() {
  data→role := "Teacher";
  if wall→ask_boolean("Do you want to make changes to the word list? There are currently " ∥ records→spellingList_table→count ∥ " words.", "") then {
    code→draw_screen;
  }
  else {
    data→role := "Student";
    wall→clear;
    wall→add_button("transport.play", "Next word");
    wall→add_button("check", "Review");
    // Ask the first question immediately
    code→learner_review_list;
    wall→prompt("Ready to begin practicing?");
    code→question;
  }
}

event tap_wall_Page_Button(item: Page_Button) {
  wall→clear;
  if $item→icon→equals("add") then {
    code→extend_word_list;
  }
  else {
    if $item→icon→equals("transport.play") then {
      code→question;
    }
    else {
      if $item→icon→equals("delete") then {
        code→clear_list;
      }
      else {
        if $item→icon→equals("check") then {
          if data→role→equals("Teacher") then {
            code→review_word_list;
          }
          else {
            code→learner_review_list;
          }
        }
      }
    }
  }
  meta private;
}

action clear_list() {
  if wall→ask_boolean("Do you want to delete the entire list?", "") then {
    records→spellingList_table→clear;
    wall→clear;
  }
  else {
    wall→clear;
    $strings := collections→create_string_collection;
    foreach s in records→spellingList_table where true do {
      $strings→add($s→word→get);
    }
    $strings→add("Back...");
    $x := wall→pick_string("Choose a word to delete", "", $strings);
    $x1 := 0;
    foreach s1 in records→spellingList_table where true do {
      if $x1 = $x then {
        $s1→delete_row;
      }
      $x1 := $x1 + 1;
    }
    wall→clear;
  }
  meta private;
}

action question() {
  wall→clear;
  $whichWord := math→round(math→random_normalized * records→spellingList_table→count);
  $x := 0;
  foreach s4 in records→spellingList_table where true do {
    if $x = $whichWord then {
      $s2 := "The next word is, " ∥ $s4→word→get ∥ ". " ∥ $s4→spelling→get;
      $s2 := $s2→concat(". As in " ∥ $s4→sentence→get);
      $s2 := $s2→concat(". " ∥ $s4→word→get);
      languages→speak("en", $s2)→play;
      time→sleep(5);
      $s1 := wall→ask_string("Type your word here");
      if $s1→equals($s4→word→get) then {
        languages→speak("en", "Hurray! That is correct")→play;
      }
      else {
        languages→speak("en", "That is not correct. Please compare your answer to the correct spelling ")→play;
        wall→clear;
        $s3 := "But you spelled it " ∥ $s1;
        $s3→post_to_wall;
        $s := "The correct spelling is " ∥ $s4→word→get;
        $s→post_to_wall;
      }
    }
    $x := $x + 1;
  }
  meta private;
}

action draw_screen() {
  wall→clear;
  wall→add_button("transport.play", "Next word");
  wall→add_button("add", "Add word");
  wall→add_button("check", "Review");
  wall→add_button("delete", "Delete");
  meta private;
}

action review_word_list() {
  records→spellingList_table→post_to_wall;
  meta private;
}
table spellingList {
  // list of words
  type = "Table";
  fields {
    word: String_field
    spelling: String_field
    sentence: String_field
  }
}

action extend_word_list() {
  $s := records→spellingList_table→add_row;
  $s→word→set(wall→ask_string("Type a word"));
  $s1 := $s→word→get;
  $s2 := "";
  for 0 ≤ i < $s1→count do {
    $s2 := $s2→concat($s1→at($i) ∥ ", ");
  }
  $s→spelling→set($s2);
  $s→sentence→set(wall→ask_string("Use it in a sentence"));
  meta private;
}

action learner_review_list() {
  foreach s in records→spellingList_table where true do {
    $s→word→get→post_to_wall;
  }
  meta private;
}
var role : String {
}

