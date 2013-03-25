meta version "v2.2,nothing";
meta name "BullsNCows";
meta icon "acorn";
meta color "#ffffdf00";
//
meta platform "current";

action prepareBoard() {
  data→brd := media→create_board(145);
  // Create digits
  $btnClr := colors→from_rgb(0.2, 0.2, 0.2);
  data→digits := data→brd→create_sprite_set;
  for 0 ≤ i < 10 do {
    $pic := media→create_picture(40, 70);
    $pic→fill_rect(0, 0, 40, 70, 0, $btnClr);
    $pic→draw_text(4, 0, $i ∥ "", 60, 0, colors→foreground);
    $digit := data→brd→create_picture($pic);
    $digit→set_pos($i * 45 + 20, 35);
    data→digits→add($digit);
  }
  // Create controls
  $pic1 := media→create_picture(100, 70);
  $pic1→fill_rect(35, 0, 50, 50, 45, $btnClr);
  $pic1→fill_rect(36, 0, 64, 70, 0, $btnClr);
  $pic1→draw_text(45, 0, "X", 65, 0, colors→foreground);
  data→backButton := data→brd→create_picture($pic1);
  data→backButton→set_pos(data→brd→width - 170, 110);
  $pic1 := media→create_picture(100, 70);
  code→roundRect($pic1, 0, 0, 100, 70, 20, colors→accent);
  $pic1→draw_text(3, 0, "GO", 65, 0, colors→foreground);
  data→goButton := data→brd→create_picture($pic1);
  data→goButton→set_pos(data→brd→width - 60, 110);
  // For answers
  data→guessesBrd := media→create_board(400);
  // To wall
  data→guessesBrd→post_to_wall;
  data→brd→post_to_wall;
  meta private;
}
var digits : Sprite_Set {
}

event tap_sprite_in_digits(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $sprite→y = 35 and data→guessDigits→count < 4 then {
    $n := data→digits→index_of($sprite);
    data→guessDigits→add($n);
    $pos := (data→guessDigits→count - 1) * 40 + 20;
    $sprite→set_pos($pos, 110);
    data→brd→update_on_wall;
  }
  meta private;
}
var brd : Board {
}

action main() {
  wall→set_title("Быки и коровы");
  wall→set_subtitle("Угадайте число из 4 разных цифр");
  code→makeTask;
  records→guesses_table→clear;
  data→guessDigits := collections→create_number_collection;
  code→prepareBoard;
}
var goButton : Sprite {
}
var guessDigits : Number_Collection {
}
var backButton : Sprite {
}

action roundRect(pic: Picture, left: Number, @top: Number, width: Number, height: Number, r: Number, color: Color) {
  $pic→fill_rect(0, $r, $width, $height - 2 * $r, 0, $color);
  $pic→fill_rect($r, 0, $width - 2 * $r, $height, 0, $color);
  $pic→fill_ellipse(0, 0, 2 * $r, 2 * $r, 0, $color);
  $pic→fill_ellipse($width - 2 * $r, 0, 2 * $r, 2 * $r, 0, $color);
  $pic→fill_ellipse(0, $height - 2 * $r, 2 * $r, 2 * $r, 0, $color);
  $pic→fill_ellipse($width - 2 * $r, $height - 2 * $r, 2 * $r, 2 * $r, 0, $color);
  meta private;
}

action nextGuess() {
  // Check guess
  $bulls := 0;
  $cows := 0;
  $pos := 0;
  $sGuess := "";
  foreach x in data→guessDigits where true do {
    $sGuess := $sGuess ∥ $x;
    if $x = data→task→at($pos) then {
      $bulls := $bulls + 1;
    }
    else {
      if data→task→contains($x) then {
        $cows := $cows + 1;
      }
    }
    $pos := $pos + 1;
  }
  // Save guess
  $rguess := records→guesses_table→add_row;
  $rguess→bulls→set($bulls);
  $rguess→cows→set($cows);
  $rguess→guess→set($sGuess);
  // Paint guesses
  $nLines := records→guesses_table→count;
  if $bulls = 4 then {
    $nLines := $nLines + 1;
  }
  $pic := media→create_picture(data→guessesBrd→width, 50 * $nLines);
  $picPos := 50 * ($nLines - 1);
  foreach g in records→guesses_table where true do {
    $pic→draw_text(0, $picPos, $g→guess→get ∥ " --> " ∥ $g→bulls→get ∥ " bulls " ∥ $g→cows→get ∥ " cows", 40, 0, colors→foreground);
    $picPos := $picPos - 50;
  }
  if $bulls < 4 then {
    code→rePrepareBoard;
    data→guessesSprite→hide;
    data→guessesSprite := data→guessesBrd→create_picture($pic);
    data→guessesSprite→set_pos(data→guessesSprite→width / 2, data→guessesSprite→height / 2);
    data→guessesBrd→update_on_wall;
  }
  else {
    $pic→draw_text(0, 0, "Вы угадали за " ∥ records→guesses_table→count ∥ " попыток", 40, 0, colors→accent);
    wall→clear;
    $pic→post_to_wall;
  }
  meta private;
}

action makeTask() {
  $digits := collections→create_number_collection;
  data→task := collections→create_number_collection;
  for 0 ≤ i < 10 do {
    $digits→add($i);
  }
  for 0 ≤ i1 < 4 do {
    $j := math→random($digits→count);
    data→task→add($digits→at($j));
    $digits→remove_at($j);
  }
  meta private;
}
var task : Number_Collection {
}

event tap_sprite\u003A_backButton(sprite: Sprite, x: Number, y: Number) {
  if data→guessDigits→count > 0 then {
    $pos := data→guessDigits→count - 1;
    $n := data→guessDigits→at($pos);
    data→guessDigits→remove_at($pos);
    data→digits→at($n)→set_pos($n * 45 + 20, 35);
    data→brd→update_on_wall;
  }
  meta private;
}

event tap_sprite\u003A_goButton(sprite: Sprite, x: Number, y: Number) {
  if data→guessDigits→count = 4 then {
    code→nextGuess;
  }
  meta private;
}
table guesses {
  type = "Table";
  fields {
    guess: String_field
    bulls: Number_field
    cows: Number_field
  }
}

action rePrepareBoard() {
  data→guessDigits→clear;
  for 0 ≤ i < 10 do {
    data→digits→at($i)→set_pos(45 * $i + 20, 35);
  }
  data→brd→update_on_wall;
  meta private;
}
var guessesSprite : Sprite {
}
var guessesBrd : Board {
}

event swipe_sprite\u003A_guessesSprite(sprite: Sprite, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  if data→guessesSprite→height > data→guessesBrd→height then {
    $sprite→set_speed_y(math→round($delta_y * 2));
    if $delta_y > 0 then {
      $sprite→set_acceleration_y( - 3);
    }
    else {
      $sprite→set_acceleration_y(3);
    }
  }
  meta private;
}

event gameloop() {
  if data→guessesSprite→y > data→guessesSprite→height / 2 and data→guessesSprite→speed_y > 0 then {
    data→guessesSprite→set_acceleration_y(0);
    data→guessesSprite→set_speed_y(0);
    data→guessesSprite→set_y(data→guessesSprite→height / 2);
  }
  if data→guessesSprite→y + data→guessesSprite→height / 2 < data→guessesBrd→height and data→guessesSprite→speed_y < 0 then {
    data→guessesSprite→set_acceleration_y(0);
    data→guessesSprite→set_speed_y(0);
    data→guessesSprite→set_y(data→guessesBrd→height - data→guessesSprite→height / 2);
  }
  if data→guessesSprite→speed_y * data→guessesSprite→acceleration_y > 0 then {
    data→guessesSprite→set_acceleration_y(0);
    data→guessesSprite→set_speed_y(0);
  }
  data→guessesBrd→evolve;
  data→guessesBrd→update_on_wall;
  meta private;
}

event tap_sprite\u003A_guessesSprite(sprite: Sprite, x: Number, y: Number) {
  $sprite→set_acceleration_y(0);
  $sprite→set_speed_y(0);
  meta private;
}

