meta version "v2.2,nothing";
meta name "Numbers game";
meta icon "calculator";
meta color "#ff007fff";
// A simulation of the numbers round from Channel 4's Countdown.
meta platform "current";

action main() {
  wall→set_background(colors→accent);
  wall→set_reversed(true);
  data→scGirls := collections→create_string_collection;
  data→scGirls→add("Carol");
  data→scGirls→add("Lily");
  data→scGirls→add("Rachel");
  data→ncNumbers := collections→create_number_collection;
  code→gameStart();
}

event tap_wall_Page_Button(item: Page_Button) {
  wall→clear_buttons;
  if $item→text→equals("Solved!") then {
    "Nice one!" →post_to_wall;
  }
  if $item→text→equals("Give up") then {
    ("Hard luck, you\'re not as smart as " ∥ data→scGirls→random ∥ ".")→post_to_wall;
  }
  wall→prompt("Play again?");
  code→gameStart;
  meta private;
}
var iTarget : Number {
}

action gameStart() {
  wall→clear;
  $scMixes := collections→create_string_collection;
  $scMixes→add("6 small");
  $scMixes→add("1 large");
  $scMixes→add("2 large");
  $scMixes→add("3 large");
  $scMixes→add("4 large");
  $scMixes→add("Surprise me!");
  $intMix := wall→pick_string("Choose your mix", data→scGirls→random→concat(" asks you which selection you would like..."), $scMixes);
  if $intMix = 5 then {
    $intMix := math→random(5);
  }
  wall→clear;
  $ncLarge := collections→create_number_collection;
  for 0 ≤ i < 4 do {
    $ncLarge→add(25 * ($i + 1));
  }
  $ncSmall := collections→create_number_collection;
  for 0 ≤ i4 < 2 do {
    for 0 ≤ i3 < 10 do {
      $ncSmall→add($i3 + 1);
    }
  }
  data→ncNumbers→clear;
  if $intMix > 0 then {
    for 0 ≤ i2 < $intMix do {
      $iIndex := math→random($ncLarge→count);
      data→ncNumbers→add($ncLarge→at($iIndex));
      $ncLarge→remove_at($iIndex);
    }
  }
  for 0 ≤ i1 < 6 - $intMix do {
    $iIndex1 := math→random($ncSmall→count);
    data→ncNumbers→add($ncSmall→at($iIndex1));
    $ncSmall→remove_at($iIndex1);
  }
  foreach x in data→ncNumbers where true do {
    $x→post_to_wall;
  }
  data→iTarget := 100 + math→random(900);
  ("The target is: " ∥ data→iTarget)→post_to_wall;
  wall→add_button("check", "Solved!");
  wall→add_button("cross", "Give up");
  meta private;
}
var scGirls : String_Collection {
}
var ncNumbers : Number_Collection {
}
table obStatistics {
  type = "Object";
}

