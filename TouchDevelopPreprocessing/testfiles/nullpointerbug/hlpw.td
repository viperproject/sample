meta version "v2.2,nothing";
meta name "mygame";
meta icon "8ball";
//
meta platform "current";

action main() {
  $x := math→random(50);
  $tf := true;
  while $tf→equals(true) do {
    $x1 := wall→ask_number("Guess my number:  (0 - 50)");
    if $x ≠ $x1 then {
      if $x < $x1 then {
        wall→prompt("Try Again! The number is lower...");
      }
      else {
        wall→prompt("Try again! The number is higher...");
      }
    }
    else {
      wall→prompt("You won!");
      wall→clear;
    }
  }
}

