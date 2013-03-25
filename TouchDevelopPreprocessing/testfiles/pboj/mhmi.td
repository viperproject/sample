meta version "v2.2,nothing";
meta name "Ohm\'s law";
meta icon "AlmostEqual";
meta color "#ffFFA500";
// A classic Ohm's Law converter.
meta platform "current";

action main() {
  // Let's start by displaying the page
  code→show;
}

action show() {
  if box→is_init then {
    // This part is run once when pushing the page. State changes are allowed here.
    wall→set_background(colors→black);
    wall→set_foreground(colors→white);
    wall→set_title("V = I * R");
    data→current := "0.05";
    data→voltage := "12";
    data→locked := "current";
    code→calculate;
  }
  if true then {
    do box {
      // This is the outer box. Adding some margins and making the fonts bigger
      box→set_margins(1, 3, 1, 4);
      box→set_horizontal_alignment(1, 1);
      box→set_font_size(3);
      do box {
        // A number box is composed of
        // * a title box which can be tapped to locked the value
        // * a number box which can be editable if the value is not locked
        code→skin_number_section;
        do box {
          // This is the title. If tapped, we lock this value.
          "V (volts)" →post_to_wall;
          box→on_tapped($handler4)
          where handler4() {
            // We update the locked variable. On the handler is finished running, the page will be displayed again.
            data→locked := "voltage";
          }
        }
        do box {
          // This is the box that shows the number
          if data→locked→equals("voltage") then {
            data→voltage→post_to_wall;
          }
          else {
            box→edit_text(data→voltage, false);
            code→skin_number_box;
            // `on editing` triggers on each key stroke which makes the app very responsive
            box→on_text_editing($handler)
            where handler(text: String) {
              data→voltage := $text;
              code→calculate();
            }
          }
        }
      }
      // We do the same for I and R
      do box {
        code→skin_number_section;
        do box {
          "I (amps)" →post_to_wall;
          box→on_tapped($handler5)
          where handler5() {
            data→locked := "current";
          }
        }
        do box {
          if data→locked→equals("current") then {
            data→current→post_to_wall;
          }
          else {
            box→edit_text(data→current, false);
            code→skin_number_box;
            box→on_text_editing($handler2)
            where handler2(text1: String) {
              data→current := $text1;
              code→calculate();
            }
          }
        }
      }
      do box {
        do box {
          "R (Ohms)" →post_to_wall;
          box→on_tapped($handler6)
          where handler6() {
            data→locked := "resistance";
          }
        }
        code→skin_number_section;
        do box {
          if data→locked→equals("resistance") then {
            data→resistance→post_to_wall;
          }
          else {
            box→edit_text(data→resistance, false);
            code→skin_number_box;
            box→on_text_editing($handler3)
            where handler3(text2: String) {
              data→resistance := $text2;
              code→calculate();
            }
          }
        }
      }
    }
  }
}
var current : String {
}
var voltage : String {
}
var resistance : String {
}

action skin_number_box() {
  // Applying the default style of an editing box
  box→set_background(colors→white);
  box→set_foreground(colors→black);
  box→set_horizontal_alignment(1, 1);
  meta private;
}

action skin_number_section() {
  // Applying the default style of a number section
  // Make it stretch horizontally
  box→set_horizontal_alignment(1, 1);
  meta private;
}

action calculate() {
  // This action calculates the locked value using the Ohm's law.
  $v := code→to_number_or_0(data→voltage);
  $i := code→to_number_or_0(data→current);
  $r := code→to_number_or_0(data→resistance);
  if data→locked→equals("resistance") then {
    data→resistance := ($v / $i)→to_string;
  }
  else {
    if data→locked→equals("current") then {
      data→current := ($v / $r)→to_string;
    }
    else {
      data→voltage := ($i * $r)→to_string;
    }
  }
  meta private;
}

action to_number_or_0(s: String) returns v: Number {
  $v := $s→to_number;
  if $v→is_invalid then {
    $v := 0;
  }
  meta private;
}
var locked : String {
}

