meta version "v2.2,nothing";
meta name "TouchCalc";
meta icon "Calculator";
meta color "#ff007FFF";
// A simple scientific calculator.
// Not fully tested, so if you see a bug please comment.
// Removed some debugging text!
// Does not fully test for legal user input format.
//
// 7 March 2013 - More bugs fixed
meta platform "accelerometer,calendar,camera,compass,gyroscope,home,location,maps,media,microphone,motion,contacts,phone,radio,orientation,search,translation,musicandsounds,editoronly";

action main() {
  // Entry point
  code→init;
  code→paint;
}

action paint() {
  // Display calculator buttons
  data→button\_rows := 6;
  data→button\_cols := 4;
  $skipX := 110;
  $skipY := 85;
  data→strings := collections→create_string_collection;
  if data→standardKeys then {
    data→strings := data→keyStd;
  }
  else {
    data→strings := data→keyAlt;
  }
  $pic := art→Blank\_100x80;
  data→buttons := data→board→create_sprite_set;
  data→but\_text := data→board→create_sprite_set;
  $posY := data→buttonTop;
  for 0 ≤ i < data→button\_rows do {
    $posX := 60;
    for 0 ≤ j < data→button\_cols do {
      $but := data→board→create_picture($pic);
      $success := data→buttons→add($but);
      $but→set_pos($posX, $posY);
      $tex := data→board→create_text(100, 20, 30, data→strings→at($i * data→button\_cols + $j));
      $tex→set_color(colors→black);
      $b := data→but\_text→add($tex);
      $tex→set_pos($posX, $posY - 10);
      $posX := $posX + $skipX;
    }
    $posY := $posY + $skipY;
  }
  data→board→post_to_wall;
  meta private;
}
var board : Board {
}
var buttons : Sprite_Set {
}
var query : Sprite {
}
var but\_text : Sprite_Set {
}
var strings : String_Collection {
}
var keyStd : String_Collection {
}

action calculate(input: String) returns output: String {
  // Convert user input string into the answer. Recursive.
  $pos := 0;
  $@break := false;
  $done := false;
  if $input→count ≤ 0 then {
    $input := "0";
    $done := true;
  }
  // process special functions - atan2() - do this before brackets
  $@break := false;
  while not $@break do {
    $atanPos := $input→index_of("atan2(", 0);
    if $atanPos = - 1 then {
      $@break := true;
    }
    else {
      $commaPos := $input→index_of(",", 0);
      $endPos := $input→index_of(")", 0);
      if $endPos > $commaPos and $commaPos > $atanPos then {
        $operandX := $input→substring($atanPos + 6, $commaPos - $atanPos - 6)→to_number;
        $operandY := $input→substring($commaPos + 1, $endPos - $commaPos - 1)→to_number;
        $answer := math→atan2($operandY, $operandX);
        $input := $input→substring(0, $atanPos) ∥ $answer→to_string ∥ $input→substring($endPos + 1, $input→count - $endPos - 1);
      }
      else {
        $@break := true;
      }
    }
  }
  // call this method recursively to process brackets
  $@break := false;
  while not $@break and not $done do {
    $rightPos := $input→index_of(")", 0);
    if $rightPos = - 1 then {
      $@break := true;
    }
    else {
      $leftPos := $input→last_index_of("(", $rightPos);
      if $leftPos = - 1 then {
        $input := data→errorStr;
        $done := true;
        $@break := true;
      }
      else {
        $bracketed := $input→substring($leftPos + 1, $rightPos - $leftPos - 1);
        $input := $input→substring(0, $leftPos) ∥ code→calculate($bracketed) ∥ $input→substring($rightPos + 1, $input→count - $rightPos - 1);
      }
    }
  }
  // find unary operator, calculate answer and replace in string
  $opNdx := 0;
  $@break := false;
  while not $@break and not $done do {
    $opStr := data→unary→at($opNdx);
    $posU := $input→index_of($opStr, 0);
    if $posU ≠ - 1 then {
      $opEnd := $posU + $opStr→count - 1;
      $opEnd := code→getOperandRight($input, $opEnd);
      $operand := $input→substring($posU + $opStr→count, $opEnd - $posU - $opStr→count)→to_number;
      $answerU := 0.0;
      if $opStr→equals("sin") then {
        $answerU := math→sin($operand);
      }
      else {
        if $opStr→equals("cos") then {
          $answerU := math→cos($operand);
        }
        else {
          if $opStr→equals("tan") then {
            $answerU := math→tan($operand);
          }
          else {
            if $opStr→equals("sqrt") then {
              $answerU := math→sqrt($operand);
            }
            else {
              if $opStr→equals("asin") then {
                $answerU := math→asin($operand);
              }
              else {
                if $opStr→equals("acos") then {
                  $answerU := math→acos($operand);
                }
                else {
                  if $opStr→equals("atan") then {
                    $answerU := math→atan($operand);
                  }
                  else {
                    if $opStr→equals("log") then {
                      $answerU := math→log10($operand);
                    }
                    else {
                      if $opStr→equals("ln") then {
                        $answerU := math→loge($operand);
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      $answerUStr := $answerU→to_string;
      $input := $input→substring(0, $posU) ∥ $answerUStr ∥ $input→substring($opEnd, $input→count - $opEnd);
    }
    else {
      $opNdx := $opNdx + 1;
      if $opNdx ≥ data→unary→count then {
        $@break := true;
      }
    }
  }
  // search left to right, evaluate ^ * / operators and replace in string
  $pos := 0;
  $posL := 0;
  $posR := 0;
  $operand1 := 0.0;
  $operand2 := 0.0;
  $answerStr := "";
  $@break := false;
  while not $@break and $pos < $input→count do {
    $current := $input→at($pos);
    if $current→equals("X") then {
      $posL := code→getOperandLeft($input, $pos);
      $operand1 := $input→substring($posL, $pos - $posL)→to_number;
      $posR := code→getOperandRight($input, $pos);
      $operand2 := $input→substring($pos + 1, $posR - $pos - 1)→to_number;
      $answerStr := ($operand1 * $operand2)→to_string;
      $input := $input→substring(0, $posL) ∥ $answerStr ∥ $input→substring($posR, $input→count - $posR);
      $pos := 0;
    }
    else {
      if $current→equals("/") then {
        $posL := code→getOperandLeft($input, $pos);
        $operand1 := $input→substring($posL, $pos - $posL)→to_number;
        $posR := code→getOperandRight($input, $pos);
        $operand2 := $input→substring($pos + 1, $posR - $pos - 1)→to_number;
        $answerStr := ($operand1 / $operand2)→to_string;
        $input := $input→substring(0, $posL) ∥ $answerStr ∥ $input→substring($posR, $input→count - $posR);
        $pos := 0;
      }
      else {
        if $current→equals("^") then {
          $posL := code→getOperandLeft($input, $pos);
          $operand1 := $input→substring($posL, $pos - $posL)→to_number;
          $posR := code→getOperandRight($input, $pos);
          $operand2 := $input→substring($pos + 1, $posR - $pos - 1)→to_number;
          $answerStr := math→pow($operand1, $operand2)→to_string;
          $input := $input→substring(0, $posL) ∥ $answerStr ∥ $input→substring($posR, $input→count - $posR);
          $pos := 0;
        }
        else {
          $pos := $pos + 1;
        }
      }
    }
  }
  // search left to right, evaluate + - operators and replace in string
  $pos := 0;
  $@break := false;
  while not $@break and $pos < $input→count do {
    $current2 := $input→at($pos);
    if $pos = 0 then {
      $pos := $pos + 1;
    }
    else {
      if $pos > 0 and not $input→at($pos - 1)→equals("E") and not $input→at($pos - 1)→equals("e") then {
        if $current2→equals("+") then {
          $posL := code→getOperandLeft($input, $pos);
          $operand1 := $input→substring($posL, $pos - $posL)→to_number;
          $posR := code→getOperandRight($input, $pos);
          $operand2 := $input→substring($pos + 1, $posR - $pos - 1)→to_number;
          $answerStr := ($operand1 + $operand2)→to_string;
          $input := $input→substring(0, $posL) ∥ $answerStr ∥ $input→substring($posR, $input→count - $posR);
          $pos := 0;
        }
        else {
          if $current2→equals("-") then {
            $posL := code→getOperandLeft($input, $pos);
            $operand1 := $input→substring($posL, $pos - $posL)→to_number;
            $posR := code→getOperandRight($input, $pos);
            $operand2 := $input→substring($pos + 1, $posR - $pos - 1)→to_number;
            $answerStr := ($operand1 - $operand2)→to_string;
            $input := $input→substring(0, $posL) ∥ $answerStr ∥ $input→substring($posR, $input→count - $posR);
            $pos := 0;
          }
          else {
            $pos := $pos + 1;
          }
        }
      }
      else {
        $pos := $pos + 1;
      }
    }
  }
  // might need to do some checking here
  $trialResult := $input→to_number;
  $output := data→errorStr;
  if not $trialResult→is_invalid then {
    $output := $trialResult→to_string;
  }
  meta private;
}
var answer : Sprite {
}

action getOperandLeft(input: String, endPos: Number) returns pos: Number {
  $pos := $endPos;
  $isNumber := true;
  while $pos > 0 and $isNumber do {
    $pos := $pos - 1;
    $current := $input→at($pos);
    if $current→is_match_regex("[0-9]") then {
      $isNumber := true;
    }
    else {
      if $current→equals(".") or $current→equals("E") or $current→equals("e") then {
        $isNumber := true;
      }
      else {
        if $current→equals("-") and $pos = 0 then {
          $isNumber := false;
        }
        else {
          if $current→equals("-") and $pos > 1 and ($input→at($pos - 1)→equals("E") or $current→equals("e")) then {
            $isNumber := true;
          }
          else {
            if $current→equals("-") and $pos > 1 and ($input→at($pos - 1)→equals("+") or $input→at($pos - 1)→equals("*") or $input→at($pos - 1)→equals("/") or $input→at($pos - 1)→equals("^")) then {
              $isNumber := true;
            }
            else {
              $pos := $pos + 1;
              $isNumber := false;
            }
          }
        }
      }
    }
  }
  meta private;
}

action getOperandRight(input: String, startPos: Number) returns pos: Number {
  $startPos := $startPos + 1;
  $pos := $startPos;
  $isNumber := true;
  while $pos < $input→count and $isNumber do {
    $current := $input→at($pos);
    if $current→is_match_regex("[0-9]") then {
      $pos := $pos + 1;
    }
    else {
      if $current→equals(".") or $current→equals("E") or $current→equals("e") then {
        $pos := $pos + 1;
      }
      else {
        if $current→equals("-") and $pos = $startPos then {
          $pos := $pos + 1;
        }
        else {
          if $current→equals("-") and ($input→at($pos - 1)→equals("E") or $input→at($pos - 1)→equals("e")) then {
            $pos := $pos + 1;
          }
          else {
            $isNumber := false;
          }
        }
      }
    }
  }
  meta private;
}

action init() {
  // Set up global data
  data→board := media→create_board(640);
  data→board→set_background(colors→black);
  data→query := data→board→create_text(100, 20, 40, "");
  data→queryXpos := 50;
  data→query→set_pos(data→queryXpos, 10);
  data→answer := data→board→create_text(100, 20, 40, "0");
  data→answer→set_pos(50, 70);
  data→buttonTop := 165;
  data→keyStd := "M+,DEL,ALT,AC,MR,(,),+,7,8,9,-,4,5,6,X,1,2,3,/,0,.,E,=" →split(",");
  data→keyAlt := "M+,DEL,ALT,AC,MR,(,),x^y,sin,asin,log,10^x,cos,acos,ln,e^x,tan,atan,sqrt,x^2,atan2,pi,1/x,=" →split(",");
  data→standardKeys := true;
  data→unary := "asin,sin,log,acos,cos,ln,atan,tan,sqrt,!" →split(",");
  data→errorStr := "Syntax Error";
  data→Memory := "";
  meta private;
}
var standardKeys : Boolean {
}
var keyAlt : String_Collection {
}
var errorStr : String {
}
var unary : String_Collection {
}
var Memory : String {
}
var Blank\_100x80 : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/agycmpvq";
}
var buttonTop : Number {
}
var button\_rows : Number {
}
var button\_cols : Number {
}

event tap_sprite_in_buttons(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  $strPressed := data→strings→at($index_in_set);
  if $strPressed→equals("=") then {
    $specialOut := code→special(data→query→text);
    $text := math→round_with_precision(code→calculate($specialOut)→to_number, 20);
    data→answer→set_text($text→to_string);
  }
  else {
    if $strPressed→equals("AC") then {
      data→query→set_text("");
      data→answer→set_text("0");
    }
    else {
      if $strPressed→equals("M+") then {
        data→Memory := data→answer→text;
      }
      else {
        if $strPressed→equals("DEL") then {
          data→query→set_text(data→query→text→substring(0, data→query→text→count - 1));
        }
        else {
          if $strPressed→equals("MR") then {
            data→query→set_text(data→query→text ∥ data→Memory);
          }
          else {
            if $strPressed→equals("ALT") then {
              if data→standardKeys then {
                data→standardKeys := false;
                code→paint;
              }
              else {
                data→standardKeys := true;
                code→paint;
              }
            }
            else {
              if $strPressed→equals("x^y") then {
                data→query→set_text(data→query→text ∥ "^");
              }
              else {
                if $strPressed→equals("10^x") then {
                  data→query→set_text(data→query→text ∥ "10^");
                }
                else {
                  if $strPressed→equals("e^x") then {
                    data→query→set_text(data→query→text ∥ "e^");
                  }
                  else {
                    if $strPressed→equals("x^2") then {
                      data→query→set_text(data→query→text ∥ "^2");
                    }
                    else {
                      if $strPressed→equals("1/x") then {
                        data→query→set_text(data→query→text ∥ "1/");
                      }
                      else {
                        if $strPressed→equals("dummy") then {
                          skip;
                        }
                        else {
                          data→query→set_text(data→query→text ∥ $strPressed);
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
  data→board→update_on_wall;
  meta private;
}

action go(value: String, places: Number) {
  $x := math→round_with_precision($value→to_number, $places);
  $x→post_to_wall;
  meta private;
}

action truncate(strInput: String, maxLength: Number) returns answer: String {
  // truncate a string representing a floating point number so that it will fit within the available space
  $pos := $strInput→index_of("e", 0);
  if $pos = - 1 then {
    $pos := $strInput→count;
  }
  $carryDigit := false;
  while $strInput→count > $maxLength or $carryDigit do {
    $carryDigit := false;
    $pos := $pos - 1;
    $digitNextLast := $strInput→at($pos - 1)→to_number;
    if $strInput→at($pos)→to_number ≥ 5 then {
      $digitNextLast := $digitNextLast + 1;
    }
    if $digitNextLast = 10 then {
      $digitNextLast := 9;
      $carryDigit := true;
    }
    $strInput := $strInput→substring(0, $pos - 1) ∥ $digitNextLast→to_string ∥ $strInput→substring($pos + 1, $strInput→count - $pos - 1);
  }
  $answer := $strInput;
  meta private;
}
var queryXpos : Number {
}

event gameloop() {
  if data→board→touched then {
    $pos := data→board→touch_current;
    if not data→touchXpos = - 1 and $pos→y < 5 then {
      data→queryXpos := data→queryXpos + $pos→x - data→touchXpos;
      data→query→set_pos(data→queryXpos, 10);
      if data→queryXpos > 50 then {
        data→queryXpos := 50;
      }
      $queryPixels := data→query→text→count * 20;
      if data→queryXpos < data→board→width - $queryPixels then {
        data→queryXpos := data→board→width - $queryPixels;
      }
      data→board→update_on_wall;
    }
    data→touchXpos := $pos→x;
  }
  else {
    data→touchXpos := - 1;
  }
  meta private;
}
var touchXpos : Number {
}

action special(specialIn: String) returns specialOut: String {
  // process special numbers - pi
  $@break := false;
  while not $@break do {
    $pos := $specialIn→index_of("pi", 0);
    if $pos = - 1 then {
      $@break := true;
    }
    else {
      $specialIn := $specialIn→substring(0, $pos) ∥ math→\u03C0→to_string ∥ $specialIn→substring($pos + 2, $specialIn→count - $pos - 2);
    }
  }
  // process special numbers - e
  $@break := false;
  while not $@break do {
    $pos1 := $specialIn→index_of("e", 0);
    if $pos1 = - 1 then {
      $@break := true;
    }
    else {
      $specialIn := $specialIn→substring(0, $pos1) ∥ math→e→to_string ∥ $specialIn→substring($pos1 + 1, $specialIn→count - $pos1 - 1);
    }
  }
  $specialOut := $specialIn;
  meta private;
}

