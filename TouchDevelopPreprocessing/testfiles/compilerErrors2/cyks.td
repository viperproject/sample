meta version "v2.2,nothing";
meta name "Phone Number Wordifier";
meta icon "ABC";
meta color "#ff008000";
//
meta platform "current,location,maps,media,contacts,search,translation,musicandsounds,webonly";

action main() {
  code→ConfigureWall;
  data→startOverButtonText := "Wordify";
  $page_button := wall→add_button("next", data→startOverButtonText);
  $page_button→post_to_wall;
  skip;
  code→DoIt;
}
var phoneNumber : String {
}
table theDigits {
  type = "Index";
  keys {
    index: Number
  }
  fields {
    digit: String_field
    Alpha1: String_field
    Alpha2: String_field
    Alpha3: String_field
    Alpha4: String_field
  }
}

action ConvertDigitToAlpha(theDigit: String, digitIndex: Number) {
  if $theDigit→equals("2") then {
    code→SetIndexItem($digitIndex, $theDigit, "A", "B", "C", "");
  }
  else {
    if $theDigit→equals("3") then {
      code→SetIndexItem($digitIndex, $theDigit, "D", "E", "F", "");
    }
    else {
      if $theDigit→equals("4") then {
        code→SetIndexItem($digitIndex, $theDigit, "G", "H", "I", "");
      }
      else {
        if $theDigit→equals("5") then {
          code→SetIndexItem($digitIndex, $theDigit, "J", "K", "L", "");
        }
        else {
          if $theDigit→equals("6") then {
            code→SetIndexItem($digitIndex, $theDigit, "M", "N", "O", "");
          }
          else {
            if $theDigit→equals("7") then {
              code→SetIndexItem($digitIndex, $theDigit, "P", "Q", "R", "S");
            }
            else {
              if $theDigit→equals("8") then {
                code→SetIndexItem($digitIndex, $theDigit, "T", "U", "V", "");
              }
              else {
                if $theDigit→equals("9") then {
                  code→SetIndexItem($digitIndex, $theDigit, "W", "X", "Y", "Z");
                }
                else {
                  if $theDigit→equals("0") then {
                    code→SetIndexItem($digitIndex, $theDigit, "O", "", "", "");
                  }
                  else {
                    if $theDigit→equals(" ") then {
                      code→SetIndexItem($digitIndex, $theDigit, "", "", "", "");
                    }
                    else {
                      if $theDigit→equals("1") then {
                        code→SetIndexItem($digitIndex, $theDigit, "I", "L", "", "");
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  meta private;
}
table bulkResults {
  type = "Table";
  fields {
    f: String_field
  }
}

action SetIndexItem(index: Number, digit: String, a1: String, a2: String, a3: String, a4: String) {
  $entry := records→theDigits_index→at($index);
  $entry→digit→set($digit);
  $entry→Alpha1→set($a1 ∥ $a2 ∥ $a3 ∥ $a4);
  meta private;
}

action CrunchWordResults(currentDepth: Number, maxDepth: Number, s: String) {
  if $currentDepth = $maxDepth then {
    code→MakeResultEntry($s ∥ records→theDigits_index→at($currentDepth)→Alpha1→get→substring(0, 1));
    code→MakeResultEntry($s ∥ records→theDigits_index→at($currentDepth)→Alpha1→get→substring(1, 1));
    code→MakeResultEntry($s ∥ records→theDigits_index→at($currentDepth)→Alpha1→get→substring(2, 1));
    code→MakeResultEntry($s ∥ records→theDigits_index→at($currentDepth)→Alpha1→get→substring(3, 1));
  }
  else {
    if $currentDepth < $maxDepth then {
      code→CrunchWordResults($currentDepth + 1, $maxDepth, $s ∥ records→theDigits_index→at($currentDepth)→Alpha1→get→substring(0, 1));
      code→CrunchWordResults($currentDepth + 1, $maxDepth, $s ∥ records→theDigits_index→at($currentDepth)→Alpha1→get→substring(1, 1));
      code→CrunchWordResults($currentDepth + 1, $maxDepth, $s ∥ records→theDigits_index→at($currentDepth)→Alpha1→get→substring(2, 1));
      code→CrunchWordResults($currentDepth + 1, $maxDepth, $s ∥ records→theDigits_index→at($currentDepth)→Alpha1→get→substring(3, 1));
    }
  }
  meta private;
}

action ScrubPhoneNumberInput(s: String) returns r: String {
  $s := $s→replace("-", "");
  $s := $s→replace("+", "");
  $s := $s→replace("(", "");
  $s := $s→replace(")", "");
  $s := $s→trim(" ");
  $r := $s;
  meta private;
}

action DoIt() {
  code→ConfigureWall;
  data→phoneNumber := "";
  records→theDigits_index→clear;
  records→bulkResults_table→clear;
  records→wordResults_index→clear;
  data→phoneNumber := wall→ask_string("Please enter a phone number to wordify:");
  data→phoneNumber := code→ScrubPhoneNumberInput(data→phoneNumber);
  data→minWordLength := wall→ask_number("What is the minimum word length you would like to search for?");
  for 0 ≤ i < data→phoneNumber→count do {
    code→ConvertDigitToAlpha(data→phoneNumber→substring($i, 1), $i);
  }
  code→CrunchWordResults(0, data→phoneNumber→count - 1, "");
  code→OutputResults;
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals(data→startOverButtonText) then {
    code→DoIt;
  }
  meta private;
}

action MakeResultEntry(s: String) {
  if $s→count > 0 then {
    $Result := records→bulkResults_table→add_row;
    $Result→f→set($s);
  }
  meta private;
}

action ConfigureWall() {
  wall→clear;
  wall→set_reversed(true);
  wall→set_title("Wordify a Phone Number");
  wall→set_subtitle("Providing leading edge wordification services since 1823.");
  meta private;
}

action OutputResults() {
  code→SetupDictionary;
  "The word combinations :" →post_to_wall;
  foreach e in records→bulkResults_table where true do {
    $word := $e→f→get;
    $wordLength := $word→count;
    for 0 ≤ i < $wordLength do {
      for 0 ≤ j < ($wordLength - $i + 1) do {
        $wordlet := $word→substring($i, $i + $j);
        if $wordlet→count > data→minWordLength - 1 then {
          if records→smallDictionary_index→at($wordlet)→entry→get→equals($wordlet) then {
            time→log($wordlet);
            records→wordResults_index→at($wordlet)→theEntry→set($wordlet);
          }
        }
      }
    }
  }
  foreach e2 in records→wordResults_index where true do {
    $e2→theEntry→get→post_to_wall;
  }
  meta private;
}
table smallDictionary {
  type = "Index";
  keys {
    key: String
  }
  fields {
    entry: String_field
  }
}

action SetupDictionary() {
  $s := "";
  if records→smallDictionary_index→count = 0 then {
    foreach e in art→smallDictionaryStorage where true do {
      if $e→equals("#") then {
        $s := $s→trim(" \t");
        records→smallDictionary_index→at($s→to_upper_case)→entry→set($s→to_upper_case);
        $s := "";
      }
      else {
        $s := $s ∥ $e;
      }
    }
  }
  meta private;
}
var startOverButtonText : String {
}
var smallDictionaryStorage : String {
  is\_resource = true;
  url = "https://drivingyourcareerstorage.blob.core.windows.net/temp/wordlist.txt";
}
table wordResults {
  type = "Index";
  keys {
    theKey: String
  }
  fields {
    theEntry: String_field
  }
}
var minWordLength : Number {
}

action ConvertAlphaToDigit(word: String) returns result: String {
  $s := "";
  foreach e in $word where true do {
    if "ABC" →contains($e) then {
      $s := $s ∥ 2;
    }
    else {
      if "DEF" →contains($e) then {
        $s := $s ∥ 3;
      }
    }
  }
  $result := $s;
  meta private;
}

