meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "BreakIt! Touch";
meta icon "ThreeColumn";
meta color "#ffe25822";
meta rootId "dyBDtNePnoad4tlQEauG4Zeq";
meta platform "current";
meta parentIds "";
// It's not a real dev platform unless there's a BreakOut clone... :-)
//
// v2.0 Updated with newer APIs
#BreakIt21
action BreakIt\u0021() {
  data→board := media→create_board(640);
  data→board→set_background(colors→black);
  data→board→set_friction(1);
  data→board→create_boundary(0);
  data→board→post_to_wall;
  data→bricks := data→board→create_sprite_set;
  data→scoreBoard := data→board→create_text(100, 20, 30, "");
  data→scoreBoard→set_y(20);
  data→score := 0;
  data→points := 0;
  data→lives := 4;
  data→livesSet := data→board→create_sprite_set;
  for 0 ≤ i < 3 do {
    $b := data→board→create_ellipse(15, 15);
    $b→set_color(colors→orange);
    $b→set_pos(($i + 1) * 20, 620);
    data→livesSet→add($b);
  }
  $pic := media→create_picture(100, 20);
  $c := colors→purple;
  $pic→fill_rect(10, 0, 80, 20, 0, $c);
  $pic→fill_ellipse(0, 0, 20, 20, 0, $c);
  $pic→fill_ellipse(80, 0, 20, 20, 0, $c);
  data→paddle := data→board→create_picture($pic);
  data→paddle→set_y(500);
  data→paddle→set_friction(0);
  data→ball := data→board→create_ellipse(30, 30);
  data→ball→set_friction(0);
  data→ball→set_color(colors→orange);
  data→ball→set_y(475);
  data→speed := 280;
  data→brickCount := code→drawBricks();
}
#board
var board : Board {
  transient = true;
}
#endGame
action endGame(score: Number, ball: Sprite) {
  $ball→set_friction(1);
  for 0 ≤ i2 < 20 do {
    $ball→set_width(($i2 + 1) * 30);
    $ball→set_height(($i2 + 1) * 30);
    data→board→update_on_wall;
  }
  wall→clear;
  $s := "Score: " ∥ $score;
  $postscore := wall→ask_boolean($s ∥ " Post to leaderboard?", "Score");
  if $postscore then {
    bazaar→post_leaderboard_score($score);
  }
  wall→clear;
  bazaar→post_leaderboard_to_wall;
  meta private;
}
#drawBricks
action drawBricks() returns(total: Number) {
  $total := 0;
  $rows := 6;
  $cols := 7;
  $topMargin := 80;
  $leftMargin := 34;
  $vpad := 40;
  $hpad := data→board→width / $cols - 1;
  for 0 ≤ i < $rows do {
    $c := colors→rand;
    for 0 ≤ i1 < $cols do {
      $total := $total + 1;
      $sprite := data→board→create_rectangle(60, 20);
      $sprite→set_color($c);
      $y := ($i * $vpad) + $topMargin;
      $x := ($i1 * $hpad) + $leftMargin;
      $sprite→set_pos($x, $y);
      data→bricks→add($sprite);
      data→board→update_on_wall;
      time→sleep(.05);
    }
  }
  meta private;
}
#bricks
var bricks : Sprite_Set {
  transient = true;
}
#ball
var ball : Sprite {
  transient = true;
}
#brickCount
var brickCount : Number {
}
#lives
var lives : Number {
}
#points
var points : Number {
}
#score
var score : Number {
}
#speed
var speed : Number {
}
#livesSet
var livesSet : Sprite_Set {
  transient = true;
}
#paddle
var paddle : Sprite {
  transient = true;
}
#scoreBoard
var scoreBoard : Sprite {
  transient = true;
}
#gameloop
event gameloop() {
  data→board→update_on_wall;
  if data→ball→speed_y = 0 `and` data→board→touched then {
    data→ball→set_speed_x(math→rand(300) - 150);
    data→ball→set_speed_y(data→speed * - 1);
  }
  if data→ball→y > 600 then {
    data→lives := data→lives - 1;
    if data→lives = 0 then {
      code→endGame(data→score, data→scoreBoard);
    }
    else {
      //:: ExpectedOutput(assert.failed)
      data→livesSet→at(data→lives - 1)→hide;
    }
    data→ball→set_speed(0, 0);
    data→ball→set_pos(data→board→width / 2, 475);
    data→paddle→set_x(data→board→width / 2);
    data→board→update_on_wall;
    time→sleep(1);
  }
  if data→ball→overlaps_with(data→paddle) then {
    data→points := 0;
    data→ball→set_y(475);
    data→ball→set_speed_x(math→rand(300) * math→sign(data→ball→speed_x + 1));
    data→ball→set_speed_y(data→ball→speed_y * - 1);
  }
  $overlaps := data→ball→overlap_with(data→bricks);
  if $overlaps→count > 0 then {
    data→points := data→points + 100;
    data→score := data→score + data→points;
    data→scoreBoard→set_text(data→score ∥ "");
    $hitBrick := $overlaps→at(0);
    data→bricks→remove($hitBrick);
    $hitBrick→set_height($hitBrick→height * .4);
    data→board→update_on_wall;
    // Updating the board mid-loop let's the app flash a tiny animation change to the user before the board has a chance to redraw again.
    $hitBrick→hide;
    data→brickCount := data→brickCount - 1;
    if data→brickCount < 1 then {
      data→ball→set_speed(0, 0);
      data→ball→set_pos(data→board→width / 2, 475);
      data→paddle→set_x(data→board→width / 2);
      data→speed := data→speed + 20;
      data→brickCount := code→drawBricks();
    }
    data→ball→set_speed_y(data→ball→speed_y * - 1);
  }
  if data→board→touched then {
    data→paddle→set_x(data→board→touch_current→x);
  }
  data→board→evolve;
  meta private;
}