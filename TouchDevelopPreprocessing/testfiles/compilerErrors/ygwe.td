meta version "v2.2,nothing";
meta name "textBuilder";
meta icon "command";
meta color "#ffff2800";
// This is a demo to build up text messages entirely from a menu system, such as could be used by people with disabilities. To get started, select the option to make changes to lists, then change word list then download words from standard URL. Then go back and select add words to start building message (which is copied to phone clipboard). If you want to import names from contacts you must set the privacy to allow non-anonymised contacts.
meta platform "accelerometer,camera,location,maps,contacts";

action main() {
  code→init;
  code→mainLoop;
  wall→prompt(data→builtString);
}

action buildMenuTree(menuItems: String_Collection, index: Number_Collection, nToDisplay: Number_Collection, maxItems: Number) returns startNode: Number {
  while $index→count > 0 do {
    $index→remove_at(0);
  }
  while $nToDisplay→count > 0 do {
    $nToDisplay→remove_at(0);
  }
  $newIndex := collections→create_number_collection;
  $newNToDisplay := collections→create_number_collection;
  $levelStart := 0;
  $nItemsForLevel := $menuItems→count;
  for 0 ≤ i < $nItemsForLevel do {
    $index→add( - 1);
    $nToDisplay→add(1);
  }
  while $nItemsForLevel > 1 or $levelStart = 0 do {
    // do not stop if just one item, make a new level first
    $oldNItems := $nItemsForLevel;
    $oldLevelStart := $levelStart;
    $levelStart := $levelStart + $oldNItems;
    $nItemsForLevel := math→floor(($oldNItems - 1) / $maxItems) + 1;
    for 0 ≤ j < $nItemsForLevel do {
      $index→add($oldLevelStart + $j * $maxItems);
      if $j < $nItemsForLevel - 1 then {
        $nToDisplay→add($maxItems);
      }
      else {
        $nToDisplay→add(math→mod($oldNItems - 1, $maxItems) + 1);
      }
    }
  }
  $startNode := $levelStart;
  meta private;
}

action selectFromMenuTree(menuItems: String_Collection, index: Number_Collection, nToDisplay: Number_Collection, startNode: Number, prompt: String, firstLabel: String) returns selection: Number {
  $thisMenu := collections→create_string_collection;
  $indexStack := collections→create_number_collection;
  $stackPos := 0;
  $indexStack→add($startNode);
  $selection := - 2;
  $s := $startNode;
  while $selection = - 2 do {
    while $thisMenu→count > 0 do {
      $thisMenu→remove_at(0);
    }
    if $stackPos = 0 then {
      $thisMenu→add($firstLabel);
    }
    else {
      $thisMenu→add("Back");
    }
    for 0 ≤ i < $nToDisplay→at($s) do {
      $startLab := "";
      $endLab := "";
      $labIndex := $index→at($s) + $i;
      while not $index→at($labIndex) = - 1 do {
        $labIndex := $index→at($labIndex);
      }
      $startLab := $menuItems→at($labIndex);
      $labIndex := $index→at($s) + $i;
      while not $index→at($labIndex) = - 1 do {
        $labIndex := $index→at($labIndex) + $nToDisplay→at($labIndex) - 1;
      }
      $endLab := $menuItems→at($labIndex);
      if not $endLab→equals($startLab) then {
        $startLab := $startLab ∥ " - " ∥ $endLab;
      }
      $thisMenu→add($startLab);
    }
    wall→clear;
    $sss := 0;
    $sss := wall→pick_string($prompt, "", $thisMenu);
    if not $sss = 0 and not $sss = - 1 then {
      $stackPos := $stackPos + 1;
      $s := $index→at($s) + $sss - 1;
      $indexStack→add($s);
      if $index→at($s) = - 1 then {
        $selection := $s;
      }
    }
    else {
      if $stackPos = 0 then {
        $selection := - 1;
      }
      else {
        $indexStack→remove_at($stackPos);
        $stackPos := $stackPos - 1;
        $s := $indexStack→at($stackPos);
      }
    }
  }
  meta private;
}

action downloadWords() {
  $strings := collections→create_string_collection;
  "Downloading words..." →post_to_wall;
  $s := web→download("http://www.davecurtis.net/tmp/sometext.txt");
  "Building word menu...";
  records→wordRecord_index→singleton→f→set(collections→create_string_collection);
  $strings := code→splitIntoWords($s, ", \n");
  code→cleanWords($strings, true);
  $strings→sort;
  if not records→wordRecord_index→singleton→f→get→is_invalid then {
    records→wordRecord_index→singleton→f→get→clear;
  }
  records→wordRecord_index→singleton→f→get→add_many($strings);
  meta private;
}
table wordRecord {
  type = "Index";
  fields {
    f: String_Collection_field
  }
}

action splitIntoWords(s: String, tokens: String) returns strings: String_Collection {
  // Just use regex to make all tokens the same
  $strings := collections→create_string_collection;
  if $tokens→count > 1 then {
    $s := $s→replace_regex("[" ∥ $tokens→substring(1, $tokens→count - 1) ∥ "]", $tokens→at(0));
  }
  foreach e2 in $s→split($tokens→at(0)) where true do {
    $strings→add($e2);
  }
  meta private;
}

action cleanWords(strings: String_Collection, makeLowerCase: Boolean) {
  $temp := collections→create_string_collection;
  $temp→add_many($strings);
  $strings→clear;
  foreach e in $temp where true do {
    $useThis := true;
    $s := $e;
    if $useThis then {
      if $s→count = 0 then {
        $useThis := false;
      }
    }
    if $useThis then {
      if $s→count = 1 then {
        if not $s→equals("I") and not $s→equals("A") and not $s→equals("a") then {
          $useThis := false;
        }
      }
    }
    if $useThis and not "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" →contains($s→at($s→count - 1)) then {
      $s := $s→substring(0, $s→count - 1);
    }
    if $useThis then {
      if $s→count = 0 then {
        $useThis := false;
      }
    }
    if $useThis then {
      if code→containsAny($s, ".\"&%?<>/=:;()*@#£\\[]}{^+|") then {
        $useThis := false;
      }
    }
    if $useThis then {
      if code→containsAny($s, "0123456789") then {
        $useThis := false;
      }
    }
    if $useThis then {
      if not "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" →contains($s→at(0)) then {
        $useThis := false;
      }
    }
    if $useThis then {
      if $makeLowerCase and not (($s→equals("I") or $s→starts_with("I\'"))) then {
        $allLC := true;
        $allUC := true;
        for 0 ≤ i < $s→count do {
          if $allUC or $allLC then {
            if "ABCDEFGHIJKLMNOPQRSTUVWXYZ" →contains($s→at($i)) then {
              $allLC := false;
            }
            else {
              if "abcdefghijklmnopqrstuvwxyz" →contains($s→at($i)) then {
                $allUC := false;
              }
            }
          }
        }
        if not $allUC and not $allLC then {
          $s := $s→to_lower_case;
        }
      }
    }
    if $useThis then {
      if not $strings→contains($s) then {
        $strings→add($s);
      }
    }
  }
  meta private;
}

action containsAny(s: String, tokens: String) returns b: Boolean {
  $b := false;
  for 0 ≤ i < $tokens→count do {
    if not $b then {
      if $s→contains($tokens→at($i)) then {
        $b := true;
      }
    }
  }
  meta private;
}
table nameRecord {
  type = "Index";
  fields {
    f: String_Collection_field
  }
}
table phraseRecord {
  type = "Index";
  fields {
    f: String_Collection_field
  }
}
table treeMenu {
  type = "Object";
  fields {
    items: String_Collection_field
    index: Number_Collection_field
    nToDisplay: Number_Collection_field
    startNode: Number_field
  }
}

action buildTreeMenu(treeMenu1: treeMenu, strings: String_Collection, maxToDisplay: Number) {
  if $treeMenu1→items→get→is_invalid then {
    $treeMenu1→items→set(collections→create_string_collection);
  }
  else {
    $treeMenu1→items→get→clear;
  }
  if $treeMenu1→index→get→is_invalid then {
    $treeMenu1→index→set(collections→create_number_collection);
  }
  else {
    $treeMenu1→index→get→clear;
  }
  if $treeMenu1→nToDisplay→get→is_invalid then {
    $treeMenu1→nToDisplay→set(collections→create_number_collection);
  }
  else {
    $treeMenu1→nToDisplay→get→clear;
  }
  $treeMenu1→items→get→add_many($strings);
  $treeMenu1→startNode→set(code→buildMenuTree($strings, $treeMenu1→index→get, $treeMenu1→nToDisplay→get, $maxToDisplay));
  meta private;
}

action selectFromTreeMenu(t: treeMenu, prompt: String, firstLabel: String) returns selection: Number {
  $selection := code→selectFromMenuTree($t→items→get, $t→index→get, $t→nToDisplay→get, $t→startNode→get, $prompt, $firstLabel);
  meta private;
}
var wordMenu : treeMenu {
}

action initAllTreeMenus() {
  data→phraseMenu := records→treeMenu→create;
  data→wordMenu := records→treeMenu→create;
  data→nameMenu := records→treeMenu→create;
  data→numberMenu := records→treeMenu→create;
  data→letterMenu := records→treeMenu→create;
  data→punctMenu := records→treeMenu→create;
  data→mainMenu := records→treeMenu→create;
  data→maintainMenu := records→treeMenu→create;
  code→fillTreeMenuFromString(data→numberMenu, "0123456789.-", 12);
  code→fillTreeMenuFromString(data→punctMenu, ".,!?:;@\"\'£$%^&*()[]{}", 10);
  code→fillTreeMenuFromString(data→letterMenu, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", 10);
  $strings := records→wordRecord_index→singleton→f→get;
  if $strings→is_invalid or $strings→count = 0 then {
    $strings := collections→create_string_collection;
    $strings→add_many(code→splitIntoWords("no words stored", " "));
  }
  code→buildTreeMenu(data→wordMenu, $strings, 10);
  $strings := records→nameRecord_index→singleton→f→get;
  if $strings→is_invalid or $strings→count = 0 then {
    $strings := collections→create_string_collection;
    $strings→add_many(code→splitIntoWords("no names stored", " "));
  }
  code→buildTreeMenu(data→nameMenu, $strings, 10);
  $strings := records→phraseRecord_index→singleton→f→get;
  if $strings→is_invalid or $strings→count = 0 then {
    $strings := collections→create_string_collection;
    $strings→add_many(code→splitIntoWords("no phrases stored", " "));
  }
  code→buildTreeMenu(data→phraseMenu, $strings, 10);
  code→buildTreeMenu(data→mainMenu, code→splitIntoWords("Special\nPunctuation\nNumbers\nLetters\nAdd phrase\nAdd names\nType in text\nClear all text\nMake changes to lists\nQuit", "\n"), 10);
  meta private;
}
var phraseMenu : treeMenu {
}
var nameMenu : treeMenu {
}
var punctMenu : treeMenu {
}
var letterMenu : treeMenu {
}
var numberMenu : treeMenu {
}

action init() {
  code→initAllTreeMenus;
  data→builtString := "";
  data→undoStack := collections→create_number_collection;
  data→undoStackPos := 0;
  data→undoStack→add(0);
  data→startingSentence := true;
  data→capsLock := false;
  meta private;
}

action fillTreeMenuFromString(treeMenu1: treeMenu, s: String, maxToDisplay: Number) {
  $strings := collections→create_string_collection;
  for 0 ≤ i < $s→count do {
    $s2 := $s→at($i);
    $strings→add($s2);
    code→buildTreeMenu($treeMenu1, $strings, $maxToDisplay);
  }
  meta private;
}
var mainMenu : treeMenu {
}
var maintainMenu : treeMenu {
}

action mainLoop() {
  $whichMenu := 0;
  $gotMenu := false;
  while not $whichMenu = - 10 do {
    $gotMenu := false;
    wall→clear;
    $whichMenu := code→selectFromTreeMenu(data→mainMenu, data→builtString, "Add words");
    if not $gotMenu and $whichMenu = - 1 then {
      $gotMenu := true;
      code→addToString(data→wordMenu, true, true, false, "Main menu");
    }
    if not $gotMenu and $whichMenu = 0 then {
      $gotMenu := true;
      code→doSpecial;
    }
    if not $gotMenu and $whichMenu = 1 then {
      $gotMenu := true;
      code→addToString(data→punctMenu, false, false, true, "Main menu");
    }
    if not $gotMenu and $whichMenu = 2 then {
      $gotMenu := true;
      code→addToString(data→numberMenu, true, false, false, "Main menu");
    }
    if not $gotMenu and $whichMenu = 3 then {
      $gotMenu := true;
      code→addToString(data→letterMenu, false, false, false, "Main menu");
    }
    if not $gotMenu and $whichMenu = 4 then {
      $gotMenu := true;
      code→addToString(data→phraseMenu, true, true, true, "Main menu");
    }
    if not $gotMenu and $whichMenu = 5 then {
      $gotMenu := true;
      code→addToString(data→nameMenu, true, true, false, "Main menu");
    }
    if not $gotMenu and $whichMenu = 6 then {
      $gotMenu := true;
      code→addTypedText;
    }
    if not $gotMenu and $whichMenu = 7 then {
      $gotMenu := true;
      data→builtString := "";
      data→startingSentence := true;
      data→undoStackPos := 0;
      data→undoStack→clear;
      data→undoStack→add(0);
    }
    if not $gotMenu and $whichMenu = 8 then {
      $gotMenu := true;
      code→makeChangesToLists;
    }
    if not $gotMenu and $whichMenu = 9 then {
      $gotMenu := true;
      $whichMenu := - 10;
    }
  }
  meta private;
}
var builtString : String {
}
var undoStackPos : Number {
}
var undoStack : Number_Collection {
}

action addToString(treeMenu1: treeMenu, precedeWithSpace: Boolean, addSpaceBetween: Boolean, onlyAddOne: Boolean, firstLabel: String) {
  $selection := 0;
  $firstOne := true;
  while not $selection = - 1 do {
    if data→builtString→count > 0 then {
      if ".?!\n" →contains(data→builtString→at(data→builtString→count - 1)) then {
        data→startingSentence := true;
      }
      else {
        data→startingSentence := false;
      }
    }
    else {
      data→startingSentence := true;
    }
    $selection := code→selectFromTreeMenu($treeMenu1, data→builtString, $firstLabel);
    skip;
    if not $selection = - 1 then {
      if $firstOne then {
        $firstOne := false;
        if $precedeWithSpace then {
          data→builtString := data→builtString ∥ " ";
        }
      }
      else {
        if not $firstOne then {
          if $addSpaceBetween then {
            data→builtString := data→builtString ∥ " ";
          }
        }
      }
      $word := $treeMenu1→items→get→at($selection);
      if data→capsLock and $addSpaceBetween then {
        $word := $word→to_upper_case;
      }
      if data→startingSentence and $addSpaceBetween then {
        $word := $word→substring(0, 1)→to_upper_case ∥ $word→substring(1, $word→count - 1);
      }
      data→builtString := data→builtString ∥ $word;
      data→undoStackPos := data→undoStackPos + 1;
      data→undoStack→add(data→builtString→count);
      data→builtString→copy_to_clipboard;
      if $onlyAddOne then {
        $selection := - 1;
      }
    }
  }
  meta private;
}

action doSpecial() {
  $specialCharMenu := records→treeMenu→create;
  code→buildTreeMenu($specialCharMenu, code→splitIntoWords("Space\nNew line\nBackspace\nToggle caps\nChange capitalisation\nTo upper case\nTo lower case\nUndo last add", "\n"), 10);
  skip;
  $selection := - 0;
  while not $selection = - 1 do {
    $selection := code→selectFromTreeMenu($specialCharMenu, data→builtString ∥ "<<<", "Main menu");
    if not $selection = - 1 then {
      if $selection = 0 then {
        data→builtString := data→builtString ∥ " ";
      }
      if $selection = 1 and data→builtString→count > 0 then {
        data→builtString := data→builtString ∥ "\n";
      }
      if $selection > - 1 and $selection < 2 then {
        data→undoStack→add(data→builtString→count);
        data→undoStackPos := data→undoStackPos + 1;
        data→builtString→copy_to_clipboard;
      }
      if $selection = 2 then {
        data→builtString := data→builtString→substring(0, data→builtString→count - 1);
        data→builtString→copy_to_clipboard;
      }
      if $selection > - 1 and $selection < 2 then {
        data→undoStack→add(data→builtString→count);
        data→undoStackPos := data→undoStackPos + 1;
        data→builtString→copy_to_clipboard;
      }
      if $selection = 3 then {
        if data→capsLock then {
          data→capsLock := false;
        }
        else {
          data→capsLock := true;
        }
      }
      if $selection > 3 and $selection < 7 and data→undoStackPos > 0 then {
        $wordStart := data→undoStack→at(data→undoStackPos - 1);
        // will probably point to the space before the word
        if not "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" →contains(data→builtString→at($wordStart)) then {
          $wordStart := $wordStart + 1;
          // just have to hope that's OK now
        }
        $left := data→builtString→substring(0, $wordStart);
        $right := data→builtString→substring($wordStart, data→builtString→count - $wordStart);
        if $selection = 4 then {
          if "abcdefghijklmnopqrstuvwxyz" →contains($right→at(0)) then {
            $right := $right→substring(0, 1)→to_upper_case ∥ $right→substring(1, $right→count - 1);
          }
          else {
            $right := $right→substring(0, 1)→to_lower_case ∥ $right→substring(1, $right→count - 1);
          }
        }
        if $selection = 5 then {
          $right := $right→to_upper_case;
        }
        skip;
        if $selection = 6 then {
          $right := $right→to_lower_case;
        }
        data→builtString := $left ∥ $right;
        data→builtString→copy_to_clipboard;
      }
      if $selection = 7 then {
        if data→undoStackPos > 0 then {
          data→builtString := data→builtString→substring(0, data→undoStack→at(data→undoStackPos - 1));
          data→undoStack→remove_at(data→undoStackPos);
          data→undoStackPos := data→undoStackPos - 1;
          data→builtString→copy_to_clipboard;
        }
      }
    }
    skip;
  }
  meta private;
}

action addTypedText() {
  $s := wall→ask_string(data→builtString ∥ "<<<");
  if $s→count > 0 then {
    data→builtString := data→builtString ∥ $s;
    data→undoStackPos := data→undoStackPos + 1;
    data→undoStack→add(data→builtString→count);
    data→builtString→copy_to_clipboard;
  }
  meta private;
}
var startingSentence : Boolean {
}
var capsLock : Boolean {
}

action makeChangesToLists() {
  $maintainMenu := records→treeMenu→create;
  code→buildTreeMenu($maintainMenu, code→splitIntoWords("Change word list\nChange phrase list\nChange name list", "\n"), 10);
  $selection := 0;
  while not $selection = - 1 do {
    $selection := code→selectFromTreeMenu($maintainMenu, "Choose list to change", "Main menu");
    if $selection = 0 then {
      code→changeWordList;
    }
    if $selection = 1 then {
      code→changePhraseList;
    }
    if $selection = 2 then {
      code→changeNameList;
    }
  }
  meta private;
}

action changeWordList() {
  $changeWordMenu := records→treeMenu→create;
  code→buildTreeMenu($changeWordMenu, code→splitIntoWords("Add words from URL\nAdd words from pasted text\nEnter individual words (preserves case)\nDelete one word\nDelete all words\nDownload words from standard URL", "\n"), 10);
  $selection := 0;
  $strings := collections→create_string_collection;
  while not $selection = - 1 do {
    if records→wordRecord_index→singleton→f→get→is_invalid then {
      records→wordRecord_index→singleton→f→set(collections→create_string_collection);
    }
    $selection := code→selectFromTreeMenu($changeWordMenu, "List contains " ∥ records→wordRecord_index→singleton→f→get→count→to_string ∥ " entries", "Back");
    $timeStr := "";
    if $selection = 0 then {
      $s := wall→ask_string("Enter URL to extract words from");
      if not $s→is_invalid then {
        $s2 := web→download($s);
        if not $s2→is_invalid and $s2→count > 0 then {
          $strings := code→splitIntoWords($s2, " ,\n");
        }
        else {
          wall→prompt("Could not download any words from: " ∥ $s);
        }
      }
    }
    if $selection = 1 then {
      $s1 := wall→ask_string("Paste words to be added");
      if not $s1→is_invalid and $s1→count > 0 then {
        $strings := code→splitIntoWords($s1, " ,\n");
        skip;
      }
      else {
        wall→prompt("Could not find any words in: " ∥ $s1);
      }
    }
    collections;
    if $selection = 5 then {
      $timeStr := $timeStr ∥ time→now→to_string;
      $s5 := web→download("http://wortschatz.uni-leipzig.de/Papers/top10000en.txt");
      if not $s5→is_invalid and $s5→count > 0 then {
        $timeStr := $timeStr ∥ time→now→to_string;
        $strings := code→splitIntoWords($s5, "\n ,");
        $timeStr := $timeStr ∥ time→now→to_string;
      }
      else {
        wall→prompt("Could not download any words from: standard URL");
      }
    }
    if $selection = 0 or $selection = 1 or $selection = 5 then {
      $timeStr := $timeStr ∥ time→now→to_string;
      code→cleanWords($strings, true);
      $timeStr := $timeStr ∥ time→now→to_string;
      foreach e in $strings where not records→wordRecord_index→singleton→f→get→contains($e) do {
        records→wordRecord_index→singleton→f→get→add($e);
      }
      $timeStr := $timeStr ∥ time→now→to_string;
    }
    if $selection = 2 then {
      $s3 := wall→ask_string("Enter a word or phrase to be treated as a single word");
      if not $s3→is_invalid and $s3→count > 0 then {
        if not records→wordRecord_index→singleton→f→get→contains($s3) then {
          records→wordRecord_index→singleton→f→get→add($s3);
        }
        else {
          wall→prompt("\"" ∥ $s3 ∥ "\"" ∥ " was already in the list");
        }
        skip;
      }
    }
    if $selection = 3 then {
      records→wordRecord_index→singleton→f→get→sort;
      code→buildTreeMenu(data→wordMenu, records→wordRecord_index→singleton→f→get, 10);
      $wordSelection := code→selectFromTreeMenu(data→wordMenu, "Select word to delete", "Back to menu");
      if not $wordSelection = - 1 then {
        records→wordRecord_index→singleton→f→get→remove_at($selection);
      }
    }
    if $selection = 4 then {
      $b := wall→ask_boolean("Delete all words?", "Are you sure?");
      if $b then {
        records→wordRecord_index→singleton→f→clear;
      }
    }
  }
  records→wordRecord_index→singleton→f→get→sort;
  if records→wordRecord_index→singleton→f→get→count > 0 then {
    code→buildTreeMenu(data→wordMenu, records→wordRecord_index→singleton→f→get, 10);
  }
  else {
    code→buildTreeMenu(data→wordMenu, code→splitIntoWords("no words added", " "), 10);
  }
  meta private;
}

action changePhraseList() {
  $changePhraseMenu := records→treeMenu→create;
  code→buildTreeMenu($changePhraseMenu, code→splitIntoWords("Add phrases from URL \nAdd phrases from pasted text\nEnter one phrase\nDelete one phrase\nDelete all phrases", "\n"), 10);
  $selection := 0;
  $strings := collections→create_string_collection;
  while not $selection = - 1 do {
    if records→phraseRecord_index→singleton→f→get→is_invalid then {
      records→phraseRecord_index→singleton→f→set(collections→create_string_collection);
    }
    $selection := code→selectFromTreeMenu($changePhraseMenu, "List contains " ∥ records→phraseRecord_index→singleton→f→get→count→to_string ∥ " entries", "Back");
    if $selection = 0 then {
      $s := wall→ask_string("Enter URL to extract phrases from (one per line)");
      if not $s→is_invalid then {
        $s2 := web→download($s);
        if not $s2→is_invalid and $s2→count > 0 then {
          $strings := code→splitIntoWords($s2, "\n");
        }
        else {
          wall→prompt("Could not download any phrases from: " ∥ $s);
        }
      }
    }
    if $selection = 1 then {
      $s1 := wall→ask_string("Paste phrases to be added (one per line)");
      if not $s1→is_invalid and $s1→count > 0 then {
        $strings := code→splitIntoWords($s1, "\n");
        skip;
      }
      else {
        wall→prompt("Could not find any phrases in: " ∥ $s1);
      }
    }
    if $selection = 0 or $selection = 1 then {
      foreach e in $strings where not records→phraseRecord_index→singleton→f→get→contains($e) do {
        records→phraseRecord_index→singleton→f→get→add($e);
      }
    }
    if $selection = 2 then {
      $s3 := wall→ask_string("Enter a phrase to add");
      if not $s3→is_invalid and $s3→count > 0 then {
        if not records→phraseRecord_index→singleton→f→get→contains($s3) then {
          records→phraseRecord_index→singleton→f→get→add($s3);
        }
        else {
          wall→prompt("\"" ∥ $s3 ∥ "\"" ∥ " was already in the list");
        }
        skip;
      }
    }
    if $selection = 3 then {
      records→phraseRecord_index→singleton→f→get→sort;
      code→buildTreeMenu(data→phraseMenu, records→phraseRecord_index→singleton→f→get, 10);
      $wordSelection := code→selectFromTreeMenu(data→phraseMenu, "Select phrase to delete", "Back to menu");
      if not $wordSelection = - 1 then {
        records→phraseRecord_index→singleton→f→get→remove_at($selection);
      }
    }
    if $selection = 4 then {
      $b := wall→ask_boolean("Delete all phrases?", "Are you sure?");
      if $b then {
        records→phraseRecord_index→singleton→f→clear;
      }
    }
  }
  records→phraseRecord_index→singleton→f→get→sort;
  if records→phraseRecord_index→singleton→f→get→count > 0 then {
    code→buildTreeMenu(data→phraseMenu, records→phraseRecord_index→singleton→f→get, 10);
  }
  else {
    code→buildTreeMenu(data→phraseMenu, code→splitIntoWords("no phrases added", " "), 10);
  }
  meta private;
}

action changeNameList() {
  $changeNameMenu := records→treeMenu→create;
  code→buildTreeMenu($changeNameMenu, code→splitIntoWords("Add names from URL \nAdd names from pasted text\nEnter one name\nDelete one name\nDelete all names\nAdd names from contacts", "\n"), 10);
  $selection := 0;
  $strings := collections→create_string_collection;
  while not $selection = - 1 do {
    if records→nameRecord_index→singleton→f→get→is_invalid then {
      records→nameRecord_index→singleton→f→set(collections→create_string_collection);
    }
    $selection := code→selectFromTreeMenu($changeNameMenu, "List contains " ∥ records→nameRecord_index→singleton→f→get→count→to_string ∥ " entries", "Back");
    if $selection = 0 then {
      $s := wall→ask_string("Enter URL to extract names from (one per line)");
      if not $s→is_invalid then {
        $s2 := web→download($s);
        if not $s2→is_invalid and $s2→count > 0 then {
          $strings := code→splitIntoWords($s2, "\n");
        }
        else {
          wall→prompt("Could not download any names from: " ∥ $s);
        }
      }
    }
    if $selection = 1 then {
      $s1 := wall→ask_string("Paste names to be added (one per line)");
      if not $s1→is_invalid and $s1→count > 0 then {
        $strings := code→splitIntoWords($s1, "\n");
        skip;
      }
      else {
        wall→prompt("Could not find any names in: " ∥ $s1);
      }
    }
    if $selection = 5 then {
      $s4 := "";
      $contacts := social→search_contacts("");
      foreach contact in $contacts where true do {
        if not $contact→first_name→is_invalid then {
          $s4 := $s4 ∥ $contact→first_name ∥ " ";
        }
        if not $contact→last_name→is_invalid then {
          $s4 := $s4 ∥ $contact→last_name ∥ " ";
        }
      }
      $strings := code→splitIntoWords($s4, " ");
    }
    if $selection = 0 or $selection = 1 or $selection = 5 then {
      code→cleanWords($strings, false);
      foreach e in $strings where not records→nameRecord_index→singleton→f→get→contains($e) do {
        records→nameRecord_index→singleton→f→get→add($e);
      }
    }
    if $selection = 2 then {
      $s3 := wall→ask_string("Enter a name to add");
      if not $s3→is_invalid and $s3→count > 0 then {
        if not records→nameRecord_index→singleton→f→get→contains($s3) then {
          records→nameRecord_index→singleton→f→get→add($s3);
        }
        else {
          wall→prompt("\"" ∥ $s3 ∥ "\"" ∥ " was already in the list");
        }
        skip;
      }
    }
    if $selection = 3 then {
      records→nameRecord_index→singleton→f→get→sort;
      code→buildTreeMenu(data→nameMenu, records→nameRecord_index→singleton→f→get, 10);
      $wordSelection := code→selectFromTreeMenu(data→nameMenu, "Select name to delete", "Back to menu");
      if not $wordSelection = - 1 then {
        records→nameRecord_index→singleton→f→get→remove_at($selection);
      }
    }
    if $selection = 4 then {
      $b := wall→ask_boolean("Delete all names?", "Are you sure?");
      if $b then {
        records→nameRecord_index→singleton→f→clear;
      }
    }
  }
  records→nameRecord_index→singleton→f→get→sort;
  if records→nameRecord_index→singleton→f→get→count > 0 then {
    code→buildTreeMenu(data→nameMenu, records→nameRecord_index→singleton→f→get, 10);
  }
  else {
    code→buildTreeMenu(data→nameMenu, code→splitIntoWords("no names added", " "), 10);
  }
  meta private;
}

action go() {
  skip;
  meta private;
}

action splitIntoWordsOld(s: String, tokens: String) returns strings: String_Collection {
  // Just use regex to make all tokens the same
  $temp := collections→create_string_collection;
  $strings := collections→create_string_collection;
  $strings→add($s);
  for 0 ≤ i < $tokens→count do {
    $temp→add_many($strings);
    $strings→clear;
    foreach e in $temp where true do {
      foreach e4 in $e→split($tokens→at($i)) where true do {
        if not $strings→contains($e4) then {
          $strings→add($e4);
        }
      }
    }
    $temp→clear;
  }
  meta private;
}

