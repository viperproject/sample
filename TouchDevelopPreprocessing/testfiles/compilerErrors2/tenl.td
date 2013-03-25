meta version "v2.2,nothing";
meta name "hangman";
meta icon "Underline";
meta color "#ffff7518";
// A simple game where you try to guess a word before time runs out!
meta platform "current";

action play() {
  $wrong_guesses_left := 7;
  $letters := "abcdefghijklmnopqrstuvwxyz";
  $word_to_guess := code→choose_word();
  $board, $alphabet, $blanks, $update := code→create_board($word_to_guess, $wrong_guesses_left);
  $count_set := $board→create_sprite_set;
  $board→post_to_wall;
  $not_guessed := true;
  while $not_guessed do {
    if $board→touched then {
      $p := $board→touch_current;
      $sprite := code→locate_tile($alphabet, $p→x, $p→y);
      if not $sprite→is_invalid then {
        $x := $alphabet→index_of($sprite);
        if $x ≤ 25 then {
          $guess := $letters→at($x);
          $didChange := code→letter_guess($word_to_guess, $guess, $blanks, $board, $count_set);
          $num := 0;
          foreach sprite1 in $blanks where true do {
            if $sprite1→color→equals(colors→chrome) then {
              $num := $num + 1;
            }
          }
          if $num = $word_to_guess→count then {
            $not_guessed := false;
          }
          if $didChange then {
            $sprite→set_color(colors→black);
            $board→update_on_wall;
          }
          if not $didChange and not $sprite→color→equals(colors→black) then {
            $sprite→set_color(colors→black);
            $wrong_guesses_left := $wrong_guesses_left - 1;
            $update→set_text("Wrong guesses left: " ∥ $wrong_guesses_left);
            $board→evolve;
            $board→update_on_wall;
          }
        }
      }
      if $wrong_guesses_left = 0 then {
        $not_guessed := false;
      }
      else {
        $board→evolve;
      }
    }
  }
  $s := "Correct answer: " ∥ $word_to_guess;
  $s→post_to_wall;
  if $wrong_guesses_left > 0 then {
    bazaar→post_leaderboard_score($wrong_guesses_left);
  }
}

action locate_tile(TileList: Sprite_Set, x: Number, y: Number) returns Tile: Sprite {
  $Tile := data→dummy;
  foreach sprite in $TileList where true do {
    if(($sprite→x - $x ≤ $sprite→width / 2) and ($x - $sprite→x ≤ $sprite→width / 2) and ($sprite→y - $y ≤ $sprite→height / 2) and ($y - $sprite→y ≤ $sprite→height / 2)) then {
      $Tile := $sprite;
    }
  }
  meta private;
}
var dummy : Sprite {
}

action create_board(word: String, guesses_left: Number) returns board1: Board, sprite_set1: Sprite_Set, blanks: Sprite_Set, update: Sprite {
  $board := media→create_board(640);
  $alphabet := $board→create_sprite_set;
  $blank_letters := $board→create_sprite_set;
  $alpha := "abcdefghijklmnopqrstuvwxyz    ";
  $count := 0;
  $curRow := 300;
  for 0 ≤ i < 5 do {
    $curCol := 30;
    for 0 ≤ i1 < 6 do {
      $sprite1 := $board→create_rectangle(50, 50);
      $sprite1→set_pos($curCol, $curRow);
      $sprite2 := $board→create_text(100, 20, 40, $alpha→at($count));
      $sprite2→set_color(colors→black);
      $sprite2→set_pos($curCol, $curRow - 10);
      $alphabet→add($sprite1);
      $count := $count + 1;
      $curCol := $curCol + 60;
    }
    $curRow := $curRow + 60;
  }
  for 0 ≤ i2 < $word→count do {
    $count1 := $i2 * 45;
    $sprite := $board→create_rectangle(40, 5);
    $sprite→set_pos(30 + $count1, 250);
    $blank_letters→add($sprite);
  }
  $sprite3 := $board→create_text(100, 20, 30, "Wrong guesses left: " ∥ $guesses_left);
  $sprite3→set_pos(50, 25);
  $update := $sprite3;
  $sprite3→hide;
  $update→show;
  $board1 := $board;
  $sprite_set1 := $alphabet;
  $blanks := $blank_letters;
  meta private;
}

action choose_word() returns word: String {
  $board := media→create_board(640);
  $sprite_set := $board→create_sprite_set;
  $sprite_set→add($board→create_text(100, 20, 40, "buzzkill"));
  $sprite_set→add($board→create_text(100, 20, 40, "croquet"));
  $sprite_set→add($board→create_text(100, 20, 40, "daiquiri"));
  $sprite_set→add($board→create_text(100, 20, 40, "fishhook"));
  $sprite_set→add($board→create_text(100, 20, 40, "galvanize"));
  $sprite_set→add($board→create_text(100, 20, 40, "guffaw"));
  $sprite_set→add($board→create_text(100, 20, 40, "haphazard"));
  $sprite_set→add($board→create_text(100, 20, 40, "nymph"));
  $sprite_set→add($board→create_text(100, 20, 40, "jaundice"));
  $sprite_set→add($board→create_text(100, 20, 40, "jawbreaker"));
  $sprite_set→add($board→create_text(100, 20, 40, "knapsack"));
  $sprite_set→add($board→create_text(100, 20, 40, "kilobyte"));
  $sprite_set→add($board→create_text(100, 20, 40, "luxury"));
  $sprite_set→add($board→create_text(100, 20, 40, "microwave"));
  $sprite_set→add($board→create_text(100, 20, 40, "quorum"));
  $sprite_set→add($board→create_text(100, 20, 40, "pneumonia"));
  $sprite_set→add($board→create_text(100, 20, 40, "microsoft"));
  $sprite_set→add($board→create_text(100, 20, 40, "research"));
  $sprite_set→add($board→create_text(100, 20, 40, "unworthy"));
  $sprite_set→add($board→create_text(100, 20, 40, "waltz"));
  $sprite_set→add($board→create_text(100, 20, 40, "yachtsman"));
  $sprite_set→add($board→create_text(100, 20, 40, "zodiac"));
  $sprite_set→add($board→create_text(100, 20, 40, "jukebox"));
  $sprite_set→add($board→create_text(100, 20, 40, "caribou"));
  $sprite_set→add($board→create_text(100, 20, 40, "kamikaze"));
  $sprite_set→add($board→create_text(100, 20, 40, "messiah"));
  $sprite_set→add($board→create_text(100, 20, 40, "twelfth"));
  $sprite_set→add($board→create_text(100, 20, 40, "gauntlet"));
  $sprite_set→add($board→create_text(100, 20, 40, "garfield"));
  $sprite_set→add($board→create_text(100, 20, 40, "grant"));
  $sprite_set→add($board→create_text(100, 20, 40, "kayak"));
  $sprite_set→add($board→create_text(100, 20, 40, "rhythm"));
  $sprite_set→add($board→create_text(100, 20, 40, "cellulose"));
  $x := math→rand($sprite_set→count);
  $word := $sprite_set→at($x)→text;
  meta private;
}

action letter_guess(word: String, letter: String, blanks: Sprite_Set, board1: Board, count_set: Sprite_Set) returns didChange\u003F: Boolean {
  $count := 0;
  $didChange\u003F := false;
  foreach s in $word where true do {
    if $s→equals($letter) then {
      $sprite := $board1→create_text(100, 20, 40, $s);
      $sprite→set_pos($blanks→at($count)→x, $blanks→at($count)→y - 35);
      $blanks→at($count)→set_color(colors→chrome);
      $count_set→add($sprite);
      $board1→update_on_wall;
      $didChange\u003F := true;
    }
    $count := $count + 1;
  }
  meta private;
}

