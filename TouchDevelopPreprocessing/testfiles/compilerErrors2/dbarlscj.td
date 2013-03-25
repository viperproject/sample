meta version "v2.2,nothing";
meta name "hang men";
//
meta platform "current";

action main() {
  code→get_word;
}

action get_word() {
  $word := wall→ask_string("Get word")→to_upper_case;
  wall→clear;
  $dash := "";
  ... ;
  foreach s in $word where true do {
    $dash := $dash ∥ "-";
  }
  $dash→post_to_wall;
  for 0 ≤ i < 9 do {
    $guess := wall→ask_string("")→to_upper_case;
    for 0 ≤ i1 < $word→count do {
      $single_word := $word→at($i1);
      if $single_word→contains($guess) then {
        $dash := $dash→substring(0, $i1) ∥ $guess ∥ $dash→substring($i1 + 1, $word→count - ($i1 + 1));
      }
    }
    if $dash→contains("-") then {
      skip;
    }
    else {
      "Win!" →post_to_wall;
      $dash→post_to_wall;
      time→stop;
    }
    $dash→post_to_wall;
  }
  "Lose!" →post_to_wall;
  meta private;
}

