meta version "v2.2,nothing";
meta name "Save the Orcs";
meta icon "runningman";
meta color "#ffa52a2a";
// Beware of the human horde
meta platform "current";

action main() {
  wall→set_title("Save the orcs");
  wall→set_subtitle("Initializing...");
  data→LastTime := time→now;
  data→FrameDelay := 75;
  data→JumpDuration := 750;
  data→SpriteWidth := 72;
  data→SpriteHeight := 90;
  ... ;
  // Splits the sprite sheet into animation objects
  $p := invalid→picture;
  data→OrcAnimation := records→AnimationFrame→create_collection;
  for 0 ≤ i < 4 do {
    $p := art→sheet→clone;
    $p→crop($i * data→SpriteWidth, 0, data→SpriteWidth, data→SpriteHeight);
    $f := records→AnimationFrame→create;
    $f→picture→set($p);
    data→OrcAnimation→add($f);
  }
  data→HumanAnimation := records→AnimationFrame→create_collection;
  for 0 ≤ i1 < 4 do {
    $p := art→sheet→clone;
    $p→crop(0, data→SpriteHeight * 2 * ($i1 + 1), $p→width, data→SpriteHeight * 2);
    $f1 := records→AnimationFrame→create;
    $f1→picture→set($p);
    data→HumanAnimation→add($f1);
  }
  data→Other := records→AnimationFrame→create_collection;
  for 0 ≤ i2 < 3 do {
    $p := art→sheet→clone;
    $p→crop($i2 * data→SpriteWidth * 2, data→SpriteHeight, data→SpriteWidth * 2, data→SpriteHeight);
    $f2 := records→AnimationFrame→create;
    $f2→picture→set($p);
    data→Other→add($f2);
  }
  for 0 ≤ i3 < 2 do {
    $p := art→sheet→clone;
    $p→crop($i3 * data→SpriteWidth + data→SpriteWidth * 4, 0, data→SpriteWidth, data→SpriteHeight);
    $f3 := records→AnimationFrame→create;
    $f3→picture→set($p);
    data→Other→add($f3);
  }
  ... ;
  // Creates the board and the sprite collections
  data→Board := media→create_portrait_board(480, 800);
  data→Board→post_to_wall;
  data→Obstacles := data→Board→create_sprite_set;
  data→Orcs := data→Board→create_sprite_set;
  data→Humans := data→Board→create_sprite_set;
  data→Options := data→Board→create_sprite_set;
  code→InitializeSprites;
  wall→set_subtitle("");
  data→Board→post_to_wall;
  code→MainScreen;
}
var LastTime : DateTime {
}

event gameloop() {
  $currentTime := time→now;
  $elapsed := $currentTime→subtract(data→LastTime) * 1000;
  data→Timer := data→Timer + $elapsed;
  if not data→Options→at(0)→is_visible then {
    code→AddObstacles($elapsed);
    code→CheckCollisions;
    code→UpdateJumps($elapsed);
  }
  if data→Timer ≥ data→FrameDelay then {
    if data→Title→is_visible then {
      data→Title→set_angle(math→random(3) - 1);
    }
    code→UpdateAnimations(true);
    data→Timer := 0;
    if not data→Options→at(0)→is_visible then {
      foreach orc in data→Orcs where $orc→is_visible do {
        if not records→Sprite_decorator→at($orc)→fallen→get then {
          data→Score := data→Score + 1;
          data→ScoreSprite→set_text(data→Score→to_string);
        }
        if $orc→y < - data→SpriteHeight then {
          $orc→hide;
          code→CheckGameEnd;
        }
      }
    }
  }
  data→Board→evolve;
  if data→Background1→y ≤ - (data→Board→height / 2) then {
    data→Background1→set_y(data→Background2→y + data→Background2→height - 1);
  }
  if data→Background2→y ≤ - (data→Board→height / 2) then {
    data→Background2→set_y(data→Background1→y + data→Background1→height - 1);
  }
  data→Board→update_on_wall;
  if data→GameEnd then {
    code→GameOver;
  }
  data→LastTime := $currentTime;
  meta private;
}
table AnimationFrame {
  type = "Object";
  fields {
    picture: Picture_field
  }
}
var OrcAnimation : AnimationFrame_Collection {
}
var FrameDelay : Number {
}
var HumanAnimation : AnimationFrame_Collection {
}
var Orcs : Sprite_Set {
}
var Humans : Sprite_Set {
}
table thing {
  type = "Decorator";
  keys {
    target: Sprite
  }
  fields {
    frame: Number_field
    jumpingTimer: Number_field
    fallen: Boolean_field
  }
}
var Board : Board {
}

action MainScreen() {
  code→Reposition;
  data→Title→show;
  foreach option in data→Options where true do {
    $option→show;
  }
  data→ScoreSprite→set_text("");
  data→Credits→show;
  data→Board→post_to_wall;
  meta private;
}

action GameScreen() {
  code→Reposition;
  data→Score := 0;
  data→Credits→hide;
  data→Title→hide;
  foreach option in data→Options where true do {
    $option→hide;
  }
  data→Board→post_to_wall;
  meta private;
}

action HighScoresScreen() {
  wall→clear;
  wall→set_title("Highscores");
  bazaar→post_leaderboard_to_wall;
  wall→prompt("Press \"Ok\" to continue...");
  code→MainScreen;
  meta private;
}
var GameEnd : Boolean {
}

action GameOver() {
  foreach obstacle in data→Obstacles where true do {
    $obstacle→delete;
    data→Obstacles→remove($obstacle);
  }
  wall→set_title("Game Over");
  if wall→ask_boolean("Score: " ∥ data→Score→to_string, "Do you want to post it to the leaderboard?") then {
    bazaar→post_leaderboard_score(data→Score);
    code→HighScoresScreen;
  }
  else {
    code→MainScreen;
  }
  meta private;
}
var SpriteWidth : Number {
}
var SpriteHeight : Number {
}

action UpdateAnimations(advance: Boolean) {
  $frame := 0;
  foreach orc in data→Orcs where $orc→is_visible do {
    $frame := records→Sprite_decorator→at($orc)→frame→get;
    if records→Sprite_decorator→at($orc)→jumpingTimer→get ≤ 0 and not records→Sprite_decorator→at($orc)→fallen→get then {
      if $advance then {
        $frame := $frame + 1;
        if $frame ≥ data→OrcAnimation→count then {
          $frame := 0;
        }
      }
      records→Sprite_decorator→at($orc)→frame→set($frame);
      $orc→set_picture(data→OrcAnimation→at($frame)→picture→get);
    }
  }
  foreach human in data→Humans where true do {
    $frame := records→Sprite_decorator→at($human)→frame→get;
    if $advance then {
      $frame := $frame + 1;
      if $frame ≥ data→HumanAnimation→count then {
        $frame := 0;
      }
    }
    records→Sprite_decorator→at($human)→frame→set($frame);
    $human→set_picture(data→HumanAnimation→at($frame)→picture→get);
  }
  meta private;
}
var Timer : Number {
}

event tap_sprite_in_Orcs(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if not data→Options→at(0)→is_visible and records→Sprite_decorator→at($sprite)→jumpingTimer→get ≤ 0 and not records→Sprite_decorator→at($sprite)→fallen→get then {
    code→StartJump($sprite);
  }
  meta private;
}
var JumpDuration : Number {
}

action UpdateJumps(elapsed: Number) {
  foreach orc in data→Orcs where $orc→is_visible do {
    $d := records→Sprite_decorator→at($orc);
    if $d→jumpingTimer→get > 0 and not $d→fallen→get then {
      if $d→jumpingTimer→get > $elapsed then {
        $d→jumpingTimer→set($d→jumpingTimer→get - $elapsed);
      }
      else {
        $d→jumpingTimer→set(0);
        $orc→set_speed_y( - data→ScrollSpeed);
      }
    }
    else {
      if $d→jumpingTimer→get ≤ 0 and not $d→fallen→get and $orc→speed_y < 0 and $orc→y ≤ 450 then {
        $orc→set_speed_y(0);
        $orc→set_y(450);
      }
    }
  }
  meta private;
}
var ScoreSprite : Sprite {
}
var Score : Number {
}

action StartJump(Orc: Sprite) {
  records→Sprite_decorator→at($Orc)→jumpingTimer→set(data→JumpDuration);
  $Orc→set_picture(data→Other→at(3)→picture→get);
  $Orc→set_speed_y(data→ScrollSpeed);
  art→JumpFX→play;
  meta private;
}
var ScrollSpeed : Number {
}
var ScoreArea : Sprite {
}

action CheckGameEnd() {
  $alive := 0;
  foreach orc in data→Orcs where $orc→is_visible do {
    $alive := $alive + 1;
  }
  if $alive ≤ 0 then {
    data→GameEnd := true;
  }
  meta private;
}
var ObstacleTimer : Number {
}
var MaxObstacleTimer : Number {
}
var Obstacles : Sprite_Set {
}

action AddObstacles(elapsed: Number) {
  data→ObstacleTimer := data→ObstacleTimer - $elapsed;
  if data→ObstacleTimer ≤ 0 then {
    $p := art→sheet→clone;
    $p→crop(data→SpriteWidth * 2 * math→random(3), data→SpriteHeight, data→SpriteWidth * 2, data→SpriteHeight);
    $obstacle := data→Board→create_picture($p);
    $obstacle→set_pos(math→random(3) * 120 + 120, data→Board→height + data→SpriteHeight / 2);
    $obstacle→set_z_index( - 5);
    $obstacle→set_speed_y( - data→ScrollSpeed);
    data→Obstacles→add($obstacle);
    data→MaxObstacleTimer := data→MaxObstacleTimer - 50;
    data→ObstacleTimer := data→MaxObstacleTimer;
    data→ScrollSpeed := data→ScrollSpeed + 0.75;
    code→UpdateSpeed;
  }
  meta private;
}

action CheckCollisions() {
  foreach obstacle in data→Obstacles where true do {
    $orcs := $obstacle→overlap_with(data→Orcs);
    foreach orc in $orcs where $orc→is_visible do {
      $d := records→Sprite_decorator→at($orc);
      if not $d→fallen→get and $d→jumpingTimer→get ≤ 0 and $obstacle→y ≥ $orc→y then {
        $d→fallen→set(true);
        $orc→set_picture(data→Other→at(4)→picture→get);
        $orc→set_speed_y( - data→ScrollSpeed);
        art→CrashFX→play;
      }
    }
    if $obstacle→y < - data→SpriteHeight then {
      $obstacle→delete;
      data→Obstacles→remove($obstacle);
    }
  }
  meta private;
}
var Background1 : Sprite {
}
var Background2 : Sprite {
}
var background : Picture {
  is\_resource = true;
  url = "http://www.trinamic.net/images/td/sto/background.png";
}
var sheet : Picture {
  is\_resource = true;
  url = "http://www.trinamic.net/images/td/sto/sheet.png";
}
var Other : AnimationFrame_Collection {
}

action UpdateSpeed() {
  $speed := - data→ScrollSpeed;
  data→Background1→set_speed_y($speed);
  data→Background2→set_speed_y($speed);
  foreach obstacle in data→Obstacles where true do {
    $obstacle→set_speed_y($speed);
  }
  foreach orc in data→Orcs where $orc→is_visible do {
    if records→Sprite_decorator→at($orc)→fallen→get then {
      $orc→set_speed_y($speed);
    }
  }
  meta private;
}
var JumpFX : Sound {
  is\_resource = true;
  url = "http://static1.grsites.com/archive/sounds/people/people069.wav";
}
var CrashFX : Sound {
  is\_resource = true;
  url = "http://static1.grsites.com/archive/sounds/cartoon/cartoon034.wav";
}
var Options : Sprite_Set {
}

event tap_sprite_in_Options(option: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $option→elasticity = 2 then {
    wall→set_title("");
    wall→set_subtitle("");
    time→stop_and_close;
  }
  else {
    if $option→elasticity = 0 then {
      code→GameScreen;
    }
    else {
      code→HighScoresScreen;
    }
  }
  meta private;
}

action InitializeSprites() {
  $p := art→background→clone;
  $p→crop(0, 0, data→Board→width, data→Board→height);
  data→Background1 := data→Board→create_picture($p);
  data→Background2 := data→Board→create_picture($p);
  data→Background1→set_z_index( - 10);
  data→Background2→set_z_index( - 10);
  for 0 ≤ i < 3 do {
    $orc := data→Board→create_picture(media→create_picture(data→SpriteWidth, data→SpriteHeight));
    data→Orcs→add($orc);
  }
  $human := data→Board→create_picture(media→create_picture(art→sheet→width, data→SpriteHeight * 2));
  records→Sprite_decorator→at($human)→frame→set(0);
  $human→set_pos(data→Board→width / 2, 100);
  data→Humans→add($human);
  data→ScoreArea := data→Board→create_rectangle(data→Board→width, 50);
  data→ScoreArea→set_color(colors→black);
  data→ScoreArea→set_opacity(0.5);
  data→ScoreArea→set_pos(data→Board→width / 2, 775);
  data→ScoreSprite := data→Board→create_text(data→Board→width, 20, 40, "");
  data→ScoreSprite→set_pos(data→Board→width / 2, 755);
  data→ScoreSprite→show;
  data→Credits := data→Board→create_text(data→Board→width, 20, 20, "sprites extracted from http://silveiraneto.net");
  data→Credits→set_pos(data→Board→width / 2, 765);
  data→Credits→show;
  for 0 ≤ i1 < 3 do {
    $p := art→sheet→clone;
    $p→crop($i1 * 144, 900, 144, 64);
    $option := data→Board→create_picture($p);
    $option→set_pos($i1 * 156 + 84, 700);
    $option→set_elasticity($i1);
    data→Options→add($option);
  }
  $p := art→sheet→clone;
  $p→crop(0, 964, 432, 96);
  data→Title := data→Board→create_picture($p);
  data→Title→set_pos(data→Board→width / 2, 300);
  code→UpdateAnimations(false);
  meta private;
}

action Reposition() {
  wall→clear;
  data→GameEnd := false;
  data→Timer := 0;
  data→ScrollSpeed := 120;
  data→MaxObstacleTimer := 3500;
  data→ObstacleTimer := data→MaxObstacleTimer * 1.5;
  $i := 0;
  data→Background1→set_pos(data→Board→width / 2, data→Board→height / 2);
  data→Background2→set_pos(data→Board→width / 2, data→Board→height * 1.5 - 1);
  data→Background1→set_speed_y( - data→ScrollSpeed);
  data→Background2→set_speed_y( - data→ScrollSpeed);
  foreach orc in data→Orcs where true do {
    records→Sprite_decorator→at($orc)→jumpingTimer→set(0);
    records→Sprite_decorator→at($orc)→frame→set(math→random(data→OrcAnimation→count));
    records→Sprite_decorator→at($orc)→fallen→set(false);
    $i := $i + 1;
    $orc→set_pos($i * 120, 450);
    $orc→set_speed(0, 0);
    $orc→set_z_index(0);
    $orc→show;
  }
  meta private;
}
var Credits : Sprite {
}
var Title : Sprite {
}

