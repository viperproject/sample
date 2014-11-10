meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "Line Runner";
meta icon "runningman";
meta color "#ff007fff";
meta rootId "y2sOJ2tQD9vpsrWNdttP4NsG";
meta platform "current";
meta parentIds "";
// Version 2.2: better jumping; changed algorithms to limit increasing difficulty. Version 2.1: Removed the superpower of slowing down and removed more code. Version 2.0: uses phone theme; added artwork; superpowers; score and scoreboard; increasing difficulty; little help that hides when the score is thirty; removed code.

action main() {
  data→score := 0;
  wall→clear;
  data→timeobstacle := time→now;
  data→board := media→create_landscape_board(800, 480);
  if colors→is_light_theme then {
    data→board→set_background(colors→white);
  }
  else {
    data→board→set_background(colors→black);
  }
  data→line := data→board→create_rectangle(800, 20);
  data→line→set_pos(400, 310);
  data→line→set_color(colors→accent);
  data→ball := data→board→create_ellipse(80, 80);
  data→ball→set_color(colors→accent);
  data→ball→set_pos(200, 260);
  data→ball→set_angular_speed(720);
  data→small := false;
  data→jumping := false;
  data→screen := data→board→create_rectangle(800, 480);
  data→screen→set_color(colors→transparent);
  data→board→post_to_wall;
  data→obstacles := collections→create_number_collection;
  data→obstacles→clear;
  data→obstacle1 := data→board→create_rectangle(20, 20);
  data→obstacle1→set_color(colors→accent);
  if math→random(2) = 0 then {
    data→obstacle1→set_pos(810, 290);
  }
  else {
    data→obstacle1→set_pos(810, 230);
  }
  data→obstacle2 := data→board→create_rectangle(20, 20);
  data→obstacle2→set_color(colors→accent);
  if math→random(2) = 0 then {
    data→obstacle2→set_pos(810, 290);
  }
  else {
    data→obstacle2→set_pos(810, 230);
  }
  data→obstacle3 := data→board→create_rectangle(20, 20);
  data→obstacle3→set_color(colors→accent);
  if math→random(2) = 0 then {
    data→obstacle3→set_pos(810, 290);
  }
  else {
    data→obstacle3→set_pos(810, 230);
  }
  data→scoretext := data→board→create_text(100, 20, 50, "Score: 0");
  data→scoretext→set_color(colors→accent);
  data→scoretext→set_pos(600, 50);
  data→helptext := data→board→create_text(100, 20, 25, "Shrink     |     Jump or unshrink");
  data→helptext→set_color(colors→accent);
  data→helptext→set_pos(345, 450);
}

var board : Board {
  transient = true;
}

event gameloop() {
  if data→obstacle1→overlaps_with(data→ball) or data→obstacle2→overlaps_with(data→ball) or data→obstacle3→overlaps_with(data→ball) then {
    code→gameover;
  }
  data→speed := - 400 - math→sqrt(data→score);
  code→ballcode;
  code→obstaclecode;
  data→scoretext→set_text("Score: " ∥ data→score);
  if data→score = 30 then {
    data→helptext→hide;
  }
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}

var line : Sprite {
  transient = true;
}

var screen : Sprite {
  transient = true;
}

var jumping : Boolean {
}

event tap_sprite\u003a_screen(sprite: Sprite, x: Number, y: Number) {
  if $x > 400 then {
    // Right side of the screen / jump
    if not data→jumping then {
      if data→small then {
        // Ball is small, becomes normal
        data→small := false;
        data→ball→set_width(80);
        data→ball→set_height(80);
        data→ball→set_pos(200, 260);
      }
      else {
        // Ball is normal, jumps
        data→jumping := true;
        data→ball→set_speed_y( - 500);
        data→ball→set_acceleration_y(1500);
      }
    }
  }
  else {
    // Left side of the screen / shrink
    if not data→jumping then {
      // Ball is normal, becomes small
      data→timesmall := time→now;
      data→ball→set_width(40);
      data→ball→set_height(40);
      data→small := true;
      data→ball→set_pos(200, 280);
    }
  }
  meta private;
}

var obstacle1 : Sprite {
  transient = true;
}

var ball : Sprite {
  transient = true;
}

var small : Boolean {
}

var timesmall : DateTime {
}

var obstacle2 : Sprite {
  transient = true;
}

var obstacle3 : Sprite {
  transient = true;
}

var obstacles : Number_Collection {
  transient = true;
}

var scoretext : Sprite {
  transient = true;
}

var speed : Number {
}

var score : Number {
}

var timeobstacle : DateTime {
}

action gameover() {
  wall→clear;
  wall→set_reversed(true);
  ("You\'re score was " ∥ data→score)→post_to_wall;
  bazaar→post_leaderboard_score(data→score);
  bazaar→post_leaderboard_to_wall;
  if wall→ask_boolean("Play again?", "") then {
    code→main;
  }
  else {
    time→stop;
  }
  meta private;
}


action obstaclecode() {
  // Resets obstacles when they are out of screen
  if data→obstacle1→x < - 10 then {
    if math→random(2) = 0 then {
      data→obstacle1→set_pos(810, 290);
    }
    else {
      data→obstacle1→set_pos(810, 230);
    }
    data→score := data→score + 10;
    data→obstacles→remove(1);
    data→obstacle1→set_speed(0, 0);
  }
  else { if data→obstacle2→x < - 10 then {
    if math→random(2) = 0 then {
      data→obstacle2→set_pos(810, 290);
    }
    else {
      data→obstacle2→set_pos(810, 230);
    }
    data→score := data→score + 10;
    data→obstacles→remove(2);
    data→obstacle2→set_speed(0, 0);
  }
  else { if data→obstacle3→x < - 10 then {
    if math→random(2) = 0 then {
      data→obstacle3→set_pos(810, 290);
    }
    else {
      data→obstacle3→set_pos(810, 230);
    }
    data→score := data→score + 10;
    data→obstacles→remove(3);
    data→obstacle3→set_speed(0, 0);
  }
  else { } } }
  // Makes obstacles move on(to) the screen
  if time→now→greater_or_equal(data→timeobstacle→add_milliseconds(900 - math→sqrt(data→score))) then {
    data→timeobstacle := time→now;
    if not data→obstacles→contains(1) then {
      data→obstacles→add(1);
      data→obstacle1→set_speed_x(data→speed);
    }
    else { if not data→obstacles→contains(2) then {
      data→obstacles→add(2);
      data→obstacle2→set_speed_x(data→speed);
    }
    else { if not data→obstacles→contains(3) then {
      data→obstacles→add(3);
      data→obstacle3→set_speed_x(data→speed);
    }
    else { } } }
  }
  meta private;
}

action ballcode() {
  // Stops jumping
  if data→jumping then {
    if data→ball→overlaps_with(data→line) or data→ball→y ≥ data→line→y then {
      data→jumping := false;
      data→ball→set_speed(0, 0);
      data→ball→set_acceleration(0, 0);
      data→ball→set_pos(200, 260);
    }
  }
  // Stops small
  if data→small and time→now→greater_or_equal(data→timesmall→add_milliseconds(400)) then {
    data→small := false;
    data→ball→set_width(80);
    data→ball→set_height(80);
    data→ball→set_pos(200, 260);
  }
  meta private;
}

var helptext : Sprite {
  transient = true;
}