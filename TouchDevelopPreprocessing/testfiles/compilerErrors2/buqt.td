meta version "v2.2,nothing";
meta name "Anagramer";
meta icon "ABC";
meta color "#ff007FFF";
//
meta platform "current";

action main() {
  data→isDebug := false;
  data→startOverButtonText := "Anagram";
  data→phoneNumber := "Word7";
  data→minWordLength := 2;
  if data→phoneNumber→is_match_regex("@[a-zA-Z]") then {
    "is word." →post_to_wall;
  }
  else {
    "not a word." →post_to_wall;
  }
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
    Alpha: String_field
  }
}

action convertDigitToAlpha(theDigit: String, digitIndex: Number) {
  if $theDigit→equals("2") then {
    code→addNewDigitSet($digitIndex, $theDigit, "ABC");
  }
  else {
    if $theDigit→equals("3") then {
      code→addNewDigitSet($digitIndex, $theDigit, "DEF");
    }
    else {
      if $theDigit→equals("4") then {
        code→addNewDigitSet($digitIndex, $theDigit, "GHI");
      }
      else {
        if $theDigit→equals("5") then {
          code→addNewDigitSet($digitIndex, $theDigit, "JKL");
        }
        else {
          if $theDigit→equals("6") then {
            code→addNewDigitSet($digitIndex, $theDigit, "MNO");
          }
          else {
            if $theDigit→equals("7") then {
              code→addNewDigitSet($digitIndex, $theDigit, "PQRS");
            }
            else {
              if $theDigit→equals("8") then {
                code→addNewDigitSet($digitIndex, $theDigit, "TUV");
              }
              else {
                if $theDigit→equals("9") then {
                  code→addNewDigitSet($digitIndex, $theDigit, "WXYZ");
                }
                else {
                  if $theDigit→equals("0") then {
                    code→addNewDigitSet($digitIndex, $theDigit, "O");
                  }
                  else {
                    if $theDigit→equals("1") then {
                      code→addNewDigitSet($digitIndex, $theDigit, "IL");
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

action addNewDigitSet(index: Number, digit: String, a1: String) {
  $entry := records→theDigits_index→at($index);
  $entry→digit→set($digit);
  $entry→Alpha→set($a1);
  meta private;
}

action crunchWordResults(currentDepth: Number, maxDepth: Number, s: String) {
  $x := records→theDigits_index→at($currentDepth)→Alpha→get;
  if $currentDepth = $maxDepth then {
    for 0 ≤ i < $x→count do {
      code→findWordsInResult($s ∥ records→theDigits_index→at($currentDepth)→Alpha→get→substring($i, 1));
    }
  }
  else {
    if $currentDepth < $maxDepth then {
      for 0 ≤ j < $x→count do {
        code→crunchWordResults($currentDepth + 1, $maxDepth, $s ∥ records→theDigits_index→at($currentDepth)→Alpha→get→substring($j, 1));
      }
    }
  }
  meta private;
}

action scrubPhoneNumberInput(s: String) returns r: String {
  $s := $s→replace("-", "");
  $s := $s→replace("+", "");
  $s := $s→replace("(", "");
  $s := $s→replace(")", "");
  $s := $s→trim(" ");
  $r := "";
  foreach e in $s where true do {
    if not $e→to_number→is_invalid then {
      $r := $r ∥ $e;
    }
  }
  if $r→count > 7 then {
    $r := $r→substring(0, 7);
  }
  code→\_debugOutput("Scrubbing PN: ", $s, $r);
  meta private;
}

action configureWall() {
  wall→set_background(colors→chrome);
  skip;
  wall→set_title("Anagram a word or set of letters");
  wall→set_subtitle("*Please note results may not include every word possible.");
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

action setupDictionary() {
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

action showGetInputs() {
  if box→is_init then {
    code→configureWall;
  }
  if true then {
    // Top page layout box
    do box {
      code→\_debugBoxBorder;
      box→set_width(box→page_width * .8);
      box→use_vertical_layout;
      box→set_margins(1, 1, 2, 1);
      do box {
        code→\_debugBoxBorder;
        box→set_text_wrapping(true, 15);
        // Intro text
        "Anagramer can take words or sets of letters and find all the different logical combonations\n\n" →post_to_wall;
        // setup phone number input.
        "Enter a word or letters:" →post_to_wall;
        do box {
          box→set_font_size(.5);
        }
        do box {
          code→styleTextBox(data→origPhoneNumber);
          box→on_text_editing($handler2)
          where handler2(text: String) {
            data→origPhoneNumber := $text;
          }
        }
        // Setup min length input
        "What is the shortest word length you want to look for?" →post_to_wall;
        do box {
          code→styleTextBox(data→minWordLength→to_string);
          box→on_text_editing($handler1)
          where handler2(text1: String) {
            if $text1→count > 0 then {
              data→minWordLength := $text1→to_number;
            }
          }
        }
      }
      // The wordify button. We want a wait indicator, but isn't possible today.
      do box {
        "Anagram" →post_to_wall;
        box→set_border(colors→black, 0.1);
        box→set_padding(.5, 2, .5, 2);
        box→on_tapped($handler)
        where handler() {
          code→controller\u002EWordifyRequest;
        }
        do box {
          "Patience, this will take a few moments." →post_to_wall;
          box→set_font_size(.5);
        }
      }
      skip;
    }
  }
}

action showWordResults() {
  if box→is_init then {
    code→configureWall;
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
                box→use_horizontal_layout;
                do box {
                  code→\_debugBoxBorder;
                  box→set_horizontal_alignment(1, 0);
                  box→set_font_size(1);
                  box→set_padding(0, .5, 0, .5);
                  box→set_width(7);
                  ($e2→theEntry→get)→post_to_wall;
                }
                do box {
                  code→\_debugBoxBorder;
                  box→set_horizontal_alignment(1, 0);
                  box→set_font_size(1);
                  box→set_padding(0, .5, 0, .5);
                  ("(" ∥ code→convertAlphaToDigit($e2→theEntry→get) ∥ ")")→post_to_wall;
                }
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
  code→setupDictionary;
  records→theDigits_index→clear;
  records→wordResults_index→clear;
  data→phoneNumber := data→origPhoneNumber;
  data→phoneNumber := code→scrubPhoneNumberInput(data→phoneNumber);
  code→\_debugOutput("prepData", "theDigits", records→theDigits_index→count→to_string);
  code→\_debugOutput("prepData", "wordResults", records→wordResults_index→count→to_string);
  code→\_debugOutput("prepData", "phoneNumber", data→phoneNumber);
  meta private;
}

action controller\u002EWordifyRequest() {
  code→prepData;
  for 0 ≤ i < data→phoneNumber→count do {
    code→convertDigitToAlpha(data→phoneNumber→substring($i, 1), $i);
  }
  code→crunchWordResults(0, data→phoneNumber→count - 1, "");
  code→showWordResults();
  meta private;
}

action findWordsInResult(word: String) {
  skip;
  $wordLength := $word→count;
  for 0 ≤ i < $wordLength do {
    for 0 ≤ j < ($wordLength - $i + 1) do {
      $wordlet := $word→substring($i, $i + $j);
      if $wordlet→count > data→minWordLength - 1 then {
        if code→isWord($wordlet) then {
          records→wordResults_index→at($wordlet)→theEntry→set($wordlet);
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
var origPhoneNumber : String {
}

action isWord(word2: String) returns b: Boolean {
  if records→smallDictionary_index→at($word2)→entry→get→equals($word2) then {
    $b := true;
  }
  else {
    $b := false;
  }
  meta private;
}

action styleTextBox(s: String) {
  code→\_debugBoxBorder;
  box→edit_text($s, false);
  box→set_width(10);
  box→set_horizontal_alignment(1, 0);
  box→set_font_size(1);
  box→set_foreground(colors→white);
  box→set_background(colors→blue);
  box→set_margins(1, 1, 1, 1);
  box→set_padding(0, .5, 0, .5);
  meta private;
}

action go() {
  skip;
  meta private;
}

action convertAlphaToDigit(s: String) returns result: String {
  $result := "";
  foreach e in $s where true do {
    if "ABC" →contains($e) then {
      $result := $result ∥ "2";
    }
    else {
      if "DEF" →contains($e) then {
        $result := $result ∥ "3";
      }
      else {
        if "GHI" →contains($e) then {
          $result := $result ∥ "4";
        }
        else {
          if "JKL" →contains($e) then {
            $result := $result ∥ "5";
          }
          else {
            if "MNO" →contains($e) then {
              $result := $result ∥ "6";
            }
            else {
              if "PQRS" →contains($e) then {
                $result := $result ∥ "7";
              }
              else {
                if "TUV" →contains($e) then {
                  $result := $result ∥ "8";
                }
                else {
                  if "WXYZ" →contains($e) then {
                    $result := $result ∥ "9";
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  skip;
  meta private;
}

action checkletter(s: String) returns b: Boolean {
  $x := collections→create_string_collection;
  $x→add("a");
  $x→add("b");
  $x→add("c");
  $x→add("d");
  $x→add("e");
  $x→add("f");
  $x→add("g");
  $x→add("h");
  $x→add("i");
  $x→add("j");
  $x→add("k");
  $x→add("l");
  $x→add("m");
  $x→add("n");
  $x→add("o");
  $x→add("p");
  $x→add("q");
  $x→add("r");
  $x→add("s");
  $x→add("t");
  $x→add("u");
  $x→add("v");
  $x→add("w");
  $x→add("x");
  $x→add("y");
  $x→add("z");
  $x→add("A");
  $x→add("B");
  $x→add("C");
  $x→add("D");
  $x→add("E");
  $x→add("F");
  $x→add("G");
  $x→add("H");
  $x→add("I");
  $x→add("J");
  $x→add("K");
  $x→add("L");
  $x→add("M");
  $x→add("N");
  $x→add("O");
  $x→add("P");
  $x→add("Q");
  $x→add("R");
  $x→add("S");
  $x→add("T");
  $x→add("U");
  $x→add("V");
  $x→add("W");
  $x→add("X");
  $x→add("Y");
  $x→add("Z");
  $b := true;
  for 0 ≤ i < $s→count + 1 do {
    if $x→contains($s→substring($i + 1, 1)) then {
      skip;
    }
    else {
      $b := false;
    }
  }
  meta private;
}

action go2() {
  skip;
  meta private;
}

