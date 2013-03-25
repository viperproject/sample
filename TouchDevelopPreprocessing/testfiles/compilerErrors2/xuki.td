meta version "v2.2,nothing";
meta name "base converter";
meta icon "CheckAlt";
meta color "#ffFFDF00";
//
meta platform "current";

action main() {
  data→binaryValue := "1010";
  data→decimalValue := code→DecimalFromBinary(data→binaryValue);
  data→hexValue := code→HexFromBinary(data→binaryValue);
  code→show;
}

action BinaryFromHex(hex: String) returns bin: String {
  $tempHex := $hex→to_upper_case;
  $pow := 0;
  $result := "";
  while $tempHex→count > 0 do {
    $temp := $tempHex→at(0);
    if $temp→equals("0") then {
      $result := $result ∥ "0000";
    }
    else {
      if $temp→equals("1") then {
        $result := $result ∥ "0001";
      }
      else {
        if $temp→equals("2") then {
          $result := $result ∥ "0010";
        }
        else {
          if $temp→equals("3") then {
            $result := $result ∥ "0011";
          }
          else {
            if $temp→equals("4") then {
              $result := $result ∥ "0100";
            }
            else {
              if $temp→equals("5") then {
                $result := $result ∥ "0101";
              }
              else {
                if $temp→equals("6") then {
                  $result := $result ∥ "0110";
                }
                else {
                  if $temp→equals("7") then {
                    $result := $result ∥ "0111";
                  }
                  else {
                    if $temp→equals("8") then {
                      $result := $result ∥ "1000";
                    }
                    else {
                      if $temp→equals("9") then {
                        $result := $result ∥ "1001";
                      }
                      else {
                        if $temp→equals("A") then {
                          $result := $result ∥ "1010";
                        }
                        else {
                          if $temp→equals("B") then {
                            $result := $result ∥ "1011";
                          }
                          else {
                            if $temp→equals("C") then {
                              $result := $result ∥ "1100";
                            }
                            else {
                              if $temp→equals("D") then {
                                $result := $result ∥ "1101";
                              }
                              else {
                                if $temp→equals("E") then {
                                  $result := $result ∥ "1110";
                                }
                                else {
                                  $result := $result ∥ "1111";
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
          }
        }
      }
    }
    time→log("temp: " ∥ $temp);
    $tempHex := $tempHex→substring(1, ($tempHex→count - 1));
  }
  $bin := $result;
  meta private;
}

action BinaryFromDecimal(dec: String) returns bin: String {
  $decNum := $dec→to_number;
  $bin := "";
  $temp := "";
  while $decNum > 0 do {
    $remainder := math→mod($decNum, 2);
    $decNum := $decNum / 2;
    if $remainder = 1 then {
      $decNum := $decNum - (1 / 2);
    }
    $temp := $temp ∥ $remainder→to_string;
  }
  foreach e in $temp where true do {
    $bin := $e ∥ $bin;
  }
  meta private;
}

action HexFromBinary(bin: String) returns hex: String {
  $tempBin := $bin;
  while math→mod($tempBin→count, 4) ≠ 0 do {
    $tempBin := "0" ∥ $tempBin;
  }
  $result := "";
  while $tempBin→count > 0 do {
    $temp := $tempBin→substring(0, 4);
    if $temp→at(0)→equals("0") then {
      if $temp→at(1)→equals("0") then {
        if $temp→at(2)→equals("0") then {
          if $temp→at(3)→equals("0") then {
            $result := $result ∥ "0";
          }
          else {
            $result := $result ∥ "1";
          }
        }
        else {
          if $temp→at(3)→equals("0") then {
            $result := $result ∥ "2";
          }
          else {
            $result := $result ∥ "3";
          }
        }
      }
      else {
        if $temp→at(2)→equals("0") then {
          if $temp→at(3)→equals("0") then {
            $result := $result ∥ "4";
          }
          else {
            $result := $result ∥ "5";
          }
        }
        else {
          if $temp→at(3)→equals("0") then {
            $result := $result ∥ "6";
          }
          else {
            $result := $result ∥ "7";
          }
        }
      }
    }
    else {
      if $temp→at(1)→equals("0") then {
        if $temp→at(2)→equals("0") then {
          if $temp→at(3)→equals("0") then {
            $result := $result ∥ "8";
          }
          else {
            $result := $result ∥ "9";
          }
        }
        else {
          if $temp→at(3)→equals("0") then {
            $result := $result ∥ "A";
          }
          else {
            $result := $result ∥ "B";
          }
        }
      }
      else {
        if $temp→at(2)→equals("0") then {
          if $temp→at(3)→equals("0") then {
            $result := $result ∥ "C";
          }
          else {
            $result := $result ∥ "D";
          }
        }
        else {
          if $temp→at(3)→equals("0") then {
            $result := $result ∥ "E";
          }
          else {
            $result := $result ∥ "F";
          }
        }
      }
    }
    time→log("temp: " ∥ $temp);
    $tempBin := $tempBin→substring(4, ($tempBin→count - 4));
  }
  $hex := $result;
  meta private;
}

action DecimalFromBinary(bin: String) returns dec: String {
  $tempBin := $bin;
  $pow := 0;
  $result := 0;
  while $tempBin→count > 0 do {
    if $tempBin→ends_with("1") then {
      $result := $result + math→pow(2, $pow);
    }
    $pow := $pow + 1;
    $tempBin := $tempBin→remove(($tempBin→count) - 1);
  }
  $dec := $result→to_string;
  meta private;
}
var binaryValue : String {
}
var hexValue : String {
}
var octalValue : String {
}
var decimalValue : String {
}

action show() {
  if box→is_init then {
    wall→set_title("Base converter");
    wall→set_background(colors→white);
  }
  if true then {
    do box {
      box→use_vertical_layout;
      do box {
        box→use_horizontal_layout;
        "Decimal: " →post_to_wall;
        do box {
          box→on_text_edited($handler)
          where handler(text: String) {
            code→updateWithBinaryValue(code→BinaryFromDecimal($text));
          }
          code→setupTextBox(data→decimalValue);
        }
      }
      do box {
        box→use_horizontal_layout;
        "Binary:    " →post_to_wall;
        do box {
          box→on_text_edited($handler3)
          where handler3(text1: String) {
            code→updateWithBinaryValue($text1);
          }
          code→setupTextBox(data→binaryValue);
        }
      }
      do box {
        box→use_horizontal_layout;
        "Hex:        " →post_to_wall;
        do box {
          box→on_text_edited($handler2)
          where handler2(text2: String) {
            code→updateWithBinaryValue(code→BinaryFromHex($text2));
          }
          code→setupTextBox(data→hexValue);
        }
      }
    }
  }
}

action setupTextBox(word: String) {
  box→edit_text($word, false);
  box→set_width(box→page_width / 10);
  box→set_vertical_alignment(0, 0);
  box→set_horizontal_alignment(0, 0);
  box→set_width(box→page_width / 6);
  box→set_border(colors→black, 0.1);
  meta private;
}

action updateWithBinaryValue(bin: String) {
  data→binaryValue := $bin;
  data→decimalValue := code→DecimalFromBinary($bin);
  data→hexValue := code→HexFromBinary($bin);
  meta private;
}

