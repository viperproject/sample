// The root of the imprecision in this program is missing context sensitivity




meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "CloudHopper";
meta icon "weather";
meta color "#ff007fff";
meta rootId "xYoRawHVH7HV2O3YYUIf4i3J";
meta platform "current";
meta parentIds "";
#board
var board : Board {
  transient = true;
}
#gameloop
event gameloop() {
  if data→run then {
    data→board→evolve;
    data→board→update_on_wall;
    //:: ExpectedOutput(assert.failed)
    $tilt := senses→acceleration_quick→scale(700);
    data→ball→set_speed_x($tilt→x);
    data→ball→set_angular_speed(math→sign($tilt→x) * 360);
    $hits := data→ball→overlap_with(data→clouds);
    if $hits→count > 0 `and` data→ball→speed_y > 200 then {
      $hit := $hits→at(0);
      $cloudNum := data→clouds→index_of($hit);
      if $cloudNum > 0 then {
        data→ball→set_y($hit→y - 20);
        if $cloudNum > 1 then {
          data→ball→set_speed_y(data→boost);
        }
        else {
          data→ball→set_speed_y(data→boost * 1.25);
        }
      }
    }
    $cloudMR := data→clouds→at(3);
    $cloudMR→move(data→cloudSpeed, 0);
    if $cloudMR→x > data→board→width then {
      $cloudMR→set_x(0);
    }
    $cloudML := data→clouds→at(5);
    $cloudML→move( - data→cloudSpeed, 0);
    if $cloudML→x < 0 then {
      $cloudML→set_x(data→board→width);
    }
    if data→ball→x > data→board→width then {
      data→ball→set_x(0);
    }
    if data→ball→x < 0 then {
      data→ball→set_x(data→board→width);
    }
    if data→ball→y > data→board→height then {
      code→endGame;
    }
    if data→ball→y ≤ data→ylimit `and` data→ball→speed_y < 10 then {
      foreach cloud in data→clouds
      do {
        $cloud→set_friction(0);
        $cloud→set_speed_y( - data→ball→speed_y);
        if $cloud→y > data→board→height then {
          $i := data→clouds→index_of($cloud) + 1;
          if $i ≥ data→numClouds then {
            $i := 0;
          }
          //:: ExpectedOutput(assert.failed)
          if data→clouds→at($i)→y > data→curSpacer `and` data→clouds→at($i)→y < data→board→height then {
            //:: ExpectedOutput(assert.failed)
            $cloud→set_y(data→clouds→at($i)→y - data→curSpacer + (math→random(30) - 15));
            $cloud→set_x(math→random(data→board→width));
          }
        }
      }
      data→ball→set_y(data→ylimit - 20);
      data→score := data→score + 10;
      data→scoreBox→set_text(data→score→to_string);
    }
    else {
      foreach cloud2 in data→clouds
      do {
        $cloud2→set_speed_y(0);
        $cloud2→set_friction(1);
      }
    }
    $level := math→floor(data→score / 1000) + 1;
    if $level > data→level then {
      data→level := $level;
      data→ball→set_picture(code→getIcon);
      if data→curSpacer < data→spacer * 2 then {
        data→curSpacer := data→curSpacer + 10;
      }
      if data→ylimit < data→board→height * 2 / 3 then {
        data→ylimit := data→ylimit + 30;
      }
      if data→cloudSpeed < 10 then {
        data→cloudSpeed := data→cloudSpeed + 1;
      }
    }
  }
  meta private;
}
#ball
var ball : Sprite {
  transient = true;
}
#makeClouds
action makeClouds() {
  data→clouds := data→board→create_sprite_set;
  data→numClouds := 8;
  data→spacer := math→floor(data→board→height / data→numClouds);
  data→curSpacer := data→spacer;
  for 0 ≤ i < data→numClouds do {
    $pic := media→create_picture(80, 35);
    $pic→fill_ellipse(0, 15, 20, 20, 0, colors→white);
    $pic→fill_ellipse(60, 15, 20, 20, 0, colors→white);
    $pic→fill_rect(12, 15, 56, 20, 0, colors→white);
    if math→mod($i, 2) = 0 then {
      $pic→fill_ellipse(10, 5, 35, 35, 0, colors→white);
      $pic→fill_ellipse(30, 0, 40, 40, 0, colors→white);
    }
    else {
      $pic→fill_ellipse(10, 0, 40, 40, 0, colors→white);
      $pic→fill_ellipse(35, 5, 35, 35, 0, colors→white);
    }
    if $i = 0 then {
      $pic→tint(colors→light_gray→darken(0.1));
    }
    $cloud := data→board→create_picture($pic);
    $cloud→set_y($i * data→spacer);
    $cloud→set_x(math→rand(data→board→width));
    $cloud→set_friction(1);
    data→clouds→add($cloud);
  }
  data→clouds→at(data→clouds→count - 1)→set_x(data→board→width / 2);
  meta private;
}
#clouds
var clouds : Sprite_Set {
  transient = true;
}
#boost
var boost : Number {
}
#main
action main() {
  data→run := true;
  data→board := media→create_full_board;
  data→board→set_background(colors→blue→lighten(0.5));
  data→board→set_gravity(0, 900);
  data→ylimit := math→floor(data→board→height / 3);
  data→scoreBox := data→board→create_text(100, 20, 40, "0");
  data→scoreBox→set_color(colors→white);
  data→scoreBox→set_y(40);
  data→scoreBox→set_friction(1);
  data→score := 0;
  data→level := 1;
  data→cloudSpeed := data→level;
  data→boost := - 700;
  code→makeIcons;
  data→ball := data→board→create_picture(code→getIcon);
  data→ball→set_friction(0);
  code→makeClouds;
  data→board→post_to_wall;
}
#score
var score : Number {
}
#scoreBox
var scoreBox : Sprite {
  transient = true;
}
#endGame
action endGame() {
  data→run := false;
  $overlay := data→board→create_rectangle(data→board→width, data→board→height);
  $overlay→set_color(colors→black→make_transparent(0));
  data→board→create_text(data→board→width, 20, 40, "Game Over");
  for 0 ≤ i < 50 do {
    $overlay→set_color(colors→black→make_transparent($i * .02));
    data→board→update_on_wall;
  }
  time→sleep(1);
  wall→clear;
  bazaar→post_leaderboard_score(data→score);
  bazaar→post_leaderboard_to_wall;
  ("Your score: " ∥ data→score)→post_to_wall;
  $b := wall→ask_boolean("Play again?", "");
  if $b then {
    code→main;
  }
  else {
    time→stop;
  }
  meta private;
}
#run
var run : Boolean {
}
#ylimit
var ylimit : Number {
}
#level
var level : Number {
}
#numClouds
var numClouds : Number {
}
#makeIcons
action makeIcons() {
  data→icon0 := media→icon("smiliehappy");
  data→icon0→tint(colors→yellow);
  data→icon1 := media→icon("clover");
  data→icon1→tint(colors→green);
  data→icon2 := media→icon("sun");
  data→icon2→tint(colors→yellow);
  data→icon3 := media→icon("snowflake");
  data→icon3→tint(colors→cyan);
  meta private;
}
#spacer
var spacer : Number {
}
#curSpacer
var curSpacer : Number {
}
#icon1
var icon1 : Picture {
  transient = true;
}
#icon2
var icon2 : Picture {
  transient = true;
}
#icon3
var icon3 : Picture {
  transient = true;
}
#icon0
var icon0 : Picture {
  transient = true;
}
#getIcon
action getIcon() returns(icon: Picture) {
  $ball := math→mod(data→level - 1, 4);
  $icon := data→icon0;
  if $ball = 1 then {
    $icon := data→icon1;
  }
  if $ball = 2 then {
    $icon := data→icon2;
  }
  if $ball = 3 then {
    $icon := data→icon3;
  }
  meta private;
}
#cloudSpeed
var cloudSpeed : Number {
}