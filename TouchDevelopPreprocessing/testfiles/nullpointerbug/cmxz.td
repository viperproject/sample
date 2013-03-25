meta version "v2.2,nothing";
meta name "Body Mass Index Calculator";
meta icon "AddCircle";
meta color "#ffDAA520";
//
meta platform "current";

action main() {
  wall→set_title("BMI Calculator");
  wall→set_background(colors→background);
  $ht := wall→ask_number("ENTER YOUR HEIGHT IN INCHES");
  $wt := wall→ask_number("ENTER YOUR WEIGHT IN POUNDS");
  wall→set_reversed(true);
  code→displayResults($ht, $wt);
}

action displayResults(ht: Number, wt: Number) {
  $bmi := code→calculate($ht, $wt);
  $tb := wall→create_text_box("Your BMI is: " →concat(math→round_with_precision($bmi, 1)→to_string), 19);
  $tb→set_background(colors→background);
  $tb→set_foreground(colors→red);
  $tb→post_to_wall;
  if $bmi < 18.5 then {
    data→s := "Underweight";
  }
  else {
    if $bmi ≥ 18.5 and $bmi ≤ 24.9 then {
      data→s := "Normal";
    }
    else {
      if $bmi ≥ 25 and $bmi ≤ 29.9 then {
        data→s := "Overweight";
      }
      else {
        data→s := "Obese";
      }
    }
  }
  $tb2 := wall→create_text_box("Your weight status is: " →concat(data→s), 19);
  $tb2→set_foreground(colors→red);
  $tb2→post_to_wall;
  meta private;
}

action calculate(ht: Number, wt: Number) returns bmi: Number {
  $bmi := ($wt * 703) / ($ht * $ht);
  meta private;
}
var s : String {
}

