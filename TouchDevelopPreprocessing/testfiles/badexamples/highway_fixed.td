meta version "v2.2,nothing";
meta name "highway racer";
meta icon "tabright";
meta color "#ff007fff";
// You are on a highway and faster than the other cars! Try not to crash!
// v. Beta 1.0
//
// Next version will include:
// • leaderboard
// • some minor changes
// • bug fixes (if bugs found)
meta platform "current";

action main() {
  data→lastLines := 0;
  data→addLines := 0;
  data→level := 1;
  data→lastAdded := 0;
  data→score := 0;
  data→boost := 1;
  data→lvlScore := 0;
  data→board := media→create_full_board;
  code→makeCarDetect;
  data→cars := data→board→create_sprite_set;
  data→board→set_background(colors→dark_gray→darken(0.5));
  data→lines := data→board→create_sprite_set;
  data→board→set_friction(0);
  code→createRoad;
  code→makePlayer;
  data→scoreText := data→board→create_text(300, 20, 40, "0");
  data→scoreText→set_pos(data→board→width / 2, 30);
  data→lastScoreAdded := 0;
  data→board→post_to_wall;
}

event gameloop() {
  data→board→evolve;
  $carSpacer := data→level * 2.5;
  data→board→update_on_wall;
  data→roadTracks := 5;
  data→scoreText→set_text(data→score→to_string);
  if $carSpacer > 10 then {
    $carSpacer := 10;
    time→log("Min car spawn time reached!");
  }
  foreach line in data→lines where true do {
    if $line→y ≥ data→board→height then {
      $line→delete;
    }
    else {
      data→lines→remove_first;
    }
  }
  if data→lastAdded + 20 + math→random(25) - $carSpacer ≤ data→score and data→cars→count < 3 + (data→level - 1) * 2 then {
    code→makeCars;
    data→lastAdded := data→score;
  }
  data→playr→set_speed_x((senses→acceleration_stable→x * 1000) * (data→boost / 2));
  data→playr→set_angle(data→playr→speed_x / 10);
  if data→playr→x < 0 then {
    data→playr→set_x(data→board→width);
  }
  if data→playr→x > data→board→width then {
    data→playr→set_x(0);
  }
  foreach car in data→cars where true do {
    ... ;
    if $car→overlaps_with(data→playr) then {
      time→log("Game over. Score: " ∥ data→score);
      code→gameOver;
    }
    if $car→y > data→board→height + 50 then {
      $car→set_y(0);
      data→cars→remove_first;
      $car→delete;
    }
  }
  if data→board→touched then {
    code→makeCars;
  }
  if data→lvlScore = 500 then {
    data→lvlScore := 0;
    data→level := data→level + 1;
    time→log("Level up! Score: " ∥ data→score);
  }
  data→lvlScore := data→lvlScore + 1;
  data→score := data→score + 1;
  data→boost := data→boost + 0.001;
  meta private;
}
var board : Board {
}
var cars : Sprite_Set {
}

action makePlayer() {
  data→playr := data→board→create_rectangle(60, 120);
  data→playr→set_color(colors→blue);
  data→playr→set_pos(data→board→width / 2, data→board→height - 80);
  ... ;
  meta private;
}
var playr : Sprite {
}
var roadTracks : Number {
}

action createRoad() {
  for 0 ≤ i < 6 do {
    $line := data→board→create_rectangle(3, data→board→height);
    $line→set_x($i * (data→board→width / data→roadTracks));
    $line→set_y(data→board→height / 2);
  }
  meta private;
}

action gameOver() {
  $overlay := data→board→create_rectangle(data→board→width, data→board→height);
  $overlay→set_color(colors→black);
  $overlay→set_opacity(0.5);
  $pic := media→create_picture(250, 50);
  $pic→draw_text(0, 0, "Game Over", 50, 0, colors→accent);
  data→board→create_picture($pic);
  data→scoreText := data→board→create_text(300, 20, 40, data→score→to_string);
  data→scoreText→set_pos(data→board→width / 2, 30);
  data→board→update_on_wall;
  while true do {
    time→sleep(1);
  }
  meta private;
}

action makeCars() {
  $length := 120;
  $long := math→random(10);
  if $long = 1 then {
    $length := 240;
  }
  $track := math→random(data→roadTracks);
  $trackPos := $track * data→board→width / data→roadTracks + 48;
  data→car := data→board→create_rectangle(60, $length);
  data→car→set_color(colors→random);
  if data→car→color→brightness < 0.5 then {
    data→car→set_color(data→car→color→lighten(0.5));
  }
  data→car→set_x($trackPos);
  data→car→set_y(0);
  data→car→set_friction(0);
  data→car→set_speed_y(240 * data→boost);
  data→car→set_text(data→score→to_string);
  data→cars→add(data→car);
  data→lastTrack := $track;
  meta private;
}
var car : Sprite {
}
var lastAdded : Number {
}
var lastTrack : Number {
}
var score : Number {
}
var scoreText : Sprite {
}
var lastScoreAdded : Number {
}

action makeCarDetect() {
  $detect1 := data→board→create_rectangle(data→board→width / 5, 200);
  meta private;
}
var boost : Number {
}
var lvlScore : Number {
}
var lines : Sprite_Set {
}
var c : Color {
  is\_resource = true;
  url = "#ffbc3e3e";
}
var lastLines : Number {
}
var addLines : Number {
}
var level : Number {
}

