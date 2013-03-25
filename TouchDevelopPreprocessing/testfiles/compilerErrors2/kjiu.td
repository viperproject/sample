meta version "v2.2,nothing";
meta name "Phone Number Wordifier";
meta icon "ABC";
meta color "#ff008000";
//
meta platform "current,location,maps,search,translation";

action main() {
  data→isDebug := false;
  data→startOverButtonText := "Wordify";
  data→phoneNumber := "555-1212";
  data→minWordLength := 2;
  code→showGetInputs;
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

action MakeResultEntry(s: String) {
  if $s→count > 0 then {
    $Result := records→bulkResults_table→add_row;
    $Result→f→set($s);
  }
  meta private;
}

action ConfigureWall() {
  wall→clear;
  wall→set_title("Wordify a Phone Number");
  wall→set_subtitle("Providing leading edge wordification services since 1823.");
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
  $dictList := art→smallDictionaryStorage→split("\n");
  if records→smallDictionary_index→count = 0 then {
    foreach e in $dictList where true do {
      $s := $e→trim(" \t");
      records→smallDictionary_index→at($s→to_upper_case)→entry→set($s→to_upper_case);
      $s := "";
    }
    code→\_debugOutput("SetupDictionary", "loaded", records→smallDictionary_index→count→to_string);
  }
  else {
    code→\_debugOutput("SetupDictionary", "reusing", records→smallDictionary_index→count→to_string);
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

action showGetInputs() {
  if box→is_init then {
    code→ConfigureWall;
  }
  if true then {
    do box {
      code→\_debugBoxBorder;
      box→set_width(box→page_width * .8);
      box→use_vertical_layout;
      box→set_margins(1, 1, 2, 1);
      do box {
        code→\_debugBoxBorder;
        box→set_text_wrapping(true, 15);
        "Phone Number Wordifier can take any whole or partial phone number and find the words hidden in it. Finding a great word in a phone number can make it easier to remember or share with someone.\n\n" →post_to_wall;
        "Enter a phone number" →post_to_wall;
        do box {
          code→\_debugBoxBorder;
          box→edit_text(data→origPhoneNumber, false);
          box→set_width(10);
          box→set_horizontal_alignment(1, 0);
          box→set_font_size(1);
          box→set_foreground(colors→white);
          box→set_background(colors→blue);
          box→set_margins(1, 1, 1, 1);
          box→set_padding(0, .5, 0, .5);
          box→on_text_editing($handler2)
          where handler2(text: String) {
            data→origPhoneNumber := $text;
          }
        }
        "What is the shortest word length you want to look for?" →post_to_wall;
        do box {
          code→\_debugBoxBorder;
          box→edit_text(data→minWordLength→to_string, false);
          box→set_width(10);
          box→set_horizontal_alignment(1, 0);
          box→set_font_size(1);
          box→set_foreground(colors→white);
          box→set_background(colors→blue);
          box→set_margins(1, 1, 1, 1);
          box→set_padding(0, .5, 0, .5);
          box→on_text_editing($handler1)
          where handler2(text1: String) {
            if $text1→count > 0 then {
              data→minWordLength := $text1→to_number;
            }
          }
        }
      }
      do box {
        "Wordify!" →post_to_wall;
        box→set_border(colors→black, 0.1);
        box→set_padding(.5, 2, .5, 2);
        box→on_tapped($handler)
        where handler() {
          code→ProcessRequest;
        }
      }
      skip;
    }
  }
}

action showWordResults() {
  if box→is_init then {
    skip;
  }
  if true then {
    if records→wordResults_index→count > 0 then {
      do box {
        code→\_debugBoxBorder;
        box→set_width(box→page_width * .8);
        box→use_vertical_layout;
        box→set_margins(1, 1, 2, 1);
        do box {
          code→\_debugBoxBorder;
          ("We looked for words that were at least " ∥ data→minWordLength ∥ " letters long in the phone number " ∥ data→origPhoneNumber ∥ ".")→post_to_wall;
          if records→wordResults_index→count > 1 then {
            ("We found " ∥ records→wordResults_index→count→to_string ∥ " words :")→post_to_wall;
          }
          else {
            "We found one word :" →post_to_wall;
          }
          do box {
            box→use_vertical_layout;
            foreach e2 in records→wordResults_index where true do {
              do box {
                code→\_debugBoxBorder;
                box→set_horizontal_alignment(1, 0);
                box→set_font_size(1);
                box→set_padding(0, .5, 0, .5);
                ($e2→theEntry→get)→post_to_wall;
              }
            }
          }
        }
        skip;
      }
    }
    else {
      "We are sorry, but we couldn\'t find any words for that number." →post_to_wall;
    }
  }
}
var isDebug : Boolean {
}

action \_debugBoxBorder() {
  if data→isDebug then {
    box→set_border(colors→random, 0.1);
  }
  meta private;
}

action prepData() {
  records→theDigits_index→clear;
  records→bulkResults_table→clear;
  records→wordResults_index→clear;
  data→phoneNumber := data→origPhoneNumber;
  data→phoneNumber := code→ScrubPhoneNumberInput(data→phoneNumber);
  code→\_debugOutput("prepData", "theDigits", records→theDigits_index→count→to_string);
  code→\_debugOutput("prepData", "bulkResults", records→bulkResults_table→count→to_string);
  code→\_debugOutput("prepData", "wordResults", records→wordResults_index→count→to_string);
  code→\_debugOutput("prepData", "phoneNumber", data→phoneNumber);
  meta private;
}

action ProcessRequest() {
  code→prepData;
  for 0 ≤ i < data→phoneNumber→count do {
    code→ConvertDigitToAlpha(data→phoneNumber→substring($i, 1), $i);
  }
  code→CrunchWordResults(0, data→phoneNumber→count - 1, "");
  code→\_debugDumpBulkResults;
  code→FindWordsInResults;
  code→showWordResults();
  meta private;
}

action FindWordsInResults() {
  code→SetupDictionary;
  foreach e in records→bulkResults_table where true do {
    $word := $e→f→get;
    $wordLength := $word→count;
    for 0 ≤ i < $wordLength do {
      for 0 ≤ j < ($wordLength - $i + 1) do {
        $wordlet := $word→substring($i, $i + $j);
        if $wordlet→count > data→minWordLength - 1 then {
          if records→smallDictionary_index→at($wordlet)→entry→get→equals($wordlet) then {
            records→wordResults_index→at($wordlet)→theEntry→set($wordlet);
          }
        }
      }
    }
  }
  meta private;
}

action \_debugOutput(module: String, itemName: String, message: String) {
  if data→isDebug then {
    $s := time→now ∥ " > " ∥ $module ∥ " > " ∥ $itemName ∥ " > " ∥ $message;
    time→log($s);
  }
  meta private;
}

action \_debugDumpBulkResults() {
  if data→isDebug then {
    time→log("bulkResults");
    foreach e in records→bulkResults_table where true do {
      time→log($e→f→get);
    }
  }
  meta private;
}
var origPhoneNumber : String {
}

