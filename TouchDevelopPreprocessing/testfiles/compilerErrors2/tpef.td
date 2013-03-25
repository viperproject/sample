meta version "v2.2,nothing";
meta name "hang men";
//
meta platform "current";
meta import turtle {
  pub "aprqa"
  usage {
  }
}

action main() {
  code→get_word;
}

action get_word() {
  data→word := "legueoflegend,Evelyn,tempo,rengar,karhus,katarina,ezreal,lux,Leona,Quinn,malphite,Vladimir,jax,shaco,twitch,akali,xinzhao,Darius,masteryi,sona,soraka,khazik,leesin,driven,Diana,vayne,Caitlyn,chogath,vi,wukong,taric,talon,sion,Ashe,anivia,mundo,draven,garen,fizz,corki,brand,galio,fiora,casiopeia,Elise,fiddlesticks,ahri,Annie,amumu,blitzcrank,irelia,gragas,hecarim,jayce,Janna,graves,when,singed,skarner,shyvana" →split(",")→random→to_upper_case;
  data→life := 0;
  $wrong_guess := "Wrong guess: ";
  "                                 |\n                                 |" →post_to_wall;
  $dash := "";
  foreach s in data→word where true do {
    $dash := $dash ∥ "-";
  }
  $dash→post_to_wall;
  for 0 ≤ i < 50 do {
    $guess := wall→ask_string("")→to_upper_case;
    wall→clear;
    $found_letter := false;
    $wrong_guess→post_to_wall;
    while $guess→count ≠ 1 do {
      $guess := wall→ask_string("Be careful! (one letter each time)")→to_upper_case;
    }
    for 0 ≤ i1 < data→word→count do {
      $single_word := data→word→at($i1);
      if $single_word→contains($guess) then {
        $dash := $dash→substring(0, $i1) ∥ $guess ∥ $dash→substring($i1 + 1, data→word→count - ($i1 + 1));
        $found_letter := true;
      }
    }
    if not $found_letter then {
      data→life := data→life + 1;
      $wrong_guess := $wrong_guess ∥ $guess;
    }
    code→stickman;
    if $dash→contains("-") then {
      skip;
    }
    else {
      "Win!" →post_to_wall;
      media→songs→random→play;
      phone→vibrate(0.5);
      $dash→post_to_wall;
      time→stop;
    }
    $dash→post_to_wall;
  }
  "Lose!" →post_to_wall;
  data→word→post_to_wall;
  meta private;
}

action stickman() {
  if data→life ≥ 8 then {
    "Lose!" →post_to_wall;
    data→word→post_to_wall;
    "|.                               /\\\n                              " →post_to_wall;
  }
  if data→life = 7 then {
    "|.                               /\n                              " →post_to_wall;
  }
  if data→life ≥ 6 then {
    "|.                               /|\\" →post_to_wall;
  }
  if data→life = 5 then {
    "|.                               /|\n                                   " →post_to_wall;
  }
  if data→life = 4 then {
    "|.                                |\n                                   " →post_to_wall;
  }
  if data→life ≥ 3 then {
    "|.                              {O}" →post_to_wall;
  }
  if data→life = 2 then {
    "|.                              {O" →post_to_wall;
  }
  if data→life = 1 then {
    "|.                              O" →post_to_wall;
  }
  if data→life ≥ 0 then {
    "                                 |\n                                 |" →post_to_wall;
  }
  meta private;
}
var life : Number {
}
var s : String {
}
var word : String {
}
var a : Picture {
  is\_resource = true;
}

