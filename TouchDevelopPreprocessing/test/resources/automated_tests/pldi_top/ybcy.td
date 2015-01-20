meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "doodle jump";
meta icon "threecolumn";
meta color "#ff007fff";
meta rootId "xZqH79WHPX38EjcnrHkmY4ox";
meta platform "current";
meta parentIds "";
// A copycat of doodle jump, except where the aim is to out jump the defending platforms.
#main
action main() {
  data→height := 0.001;
  data→game_ended2 := false;
  code→create_board;
  code→create_platforms;
  code→create_ball;
  code→create_saftey_net;
  code→create_score;
  data→board→post_to_wall;
}
#board
var board : Board {
  transient = true;
}
#ball
var ball : Sprite {
  transient = true;
}
#platforms
var platforms : Sprite_Set {
  transient = true;
}
#gameloop
event gameloop() {
  data→last_position := data→ball→y;
  data→board→evolve;
  data→new_position := data→ball→y;
  data→score→set_text(math→round(data→height)→to_string);
  code→tilt_ball;
  code→update_platforms;
  code→ball_bounce;
  code→check_for_death;
  data→board→update_on_wall;
  meta private;
}
#createplatforms
action create_platforms() {
  $pic := art→platform_image→clone;
  $pic→resize(60, 15);
  data→platforms := data→board→create_sprite_set;
  for 0 ≤ i < 3 do {
    $platform := data→board→create_picture($pic);
    skip;
    $platform→set_friction(1);
    $platform→set_pos(math→random(305) + 37, math→random(800));
    data→platforms→add($platform);
  }
  for 0 ≤ i1 < 5 do {
    $platform1 := data→board→create_picture($pic);
    skip;
    $platform1→set_friction(1);
    $platform1→set_pos(math→random(305) + 37, 160 * $i1);
    data→platforms→add($platform1);
  }
  meta private;
}
#createball
action create_ball() {
  data→picright := art→doodle_jump2→clone;
  data→picright→resize(60, 60);
  data→ball := data→board→create_picture(data→picright);
  data→ball→set_width(30);
  meta private;
}
#ballbounce
action ball_bounce() {
  if data→ball→speed_y ≥ 0 then {
    foreach sprite in data→ball→overlap_with(data→platforms)
    do {
      data→ball→set_speed_y( - 1150);
      skip;
    }
  }
  meta private;
}
#tiltball
action tilt_ball() {
  //:: ExpectedOutput(assert.failed)
  data→ball→set_speed_x(senses→acceleration_quick→scale(1500)→x);
  if data→ball→x < 0 then {
    data→ball→set_x(480);
  }
  else { if data→ball→x > 480 then {
    data→ball→set_x(0);
  }
  else { } }
  meta private;
}
#updateplatforms
action update_platforms() {
  data→height := data→height + data→height / 500;
  data→ball→move(0, math→sqrt(data→height / 15));
  foreach sprite1 in data→platforms
  do {
    $sprite1→move(0, math→sqrt(data→height / 15));
    if $sprite1→y > 800 then {
      $sprite1→move(0, - 800);
      $sprite1→set_x(math→random(415) + 37);
    }
  }
  data→safety_net→set_pos(240, 900);
  if data→ball→speed_y < 0 then {
    if data→ball→y ≤ 250 then {
      data→height := data→height + 1;
      data→safety_net→delete;
      data→ball→set_y(data→ball→y + (data→last_position - data→new_position) * 0.75);
      skip;
      foreach sprite in data→platforms
      do {
        $sprite→set_y($sprite→y + (data→last_position - data→new_position));
      }
    }
  }
  meta private;
}
#checkfordeath
action check_for_death() {
  if data→ball→y > 900 then {
    code→game_ended;
  }
  meta private;
}
#gameended
action game_ended() {
  if data→game_ended2→equals(false) then {
    wall→push_new_page;
    wall→create_text_box("you scored " ∥ math→round(data→height)→to_string, 23)→post_to_wall;
    $b := wall→ask_boolean("Post score to leaderboard?", "");
    if $b→equals(true) then {
      bazaar→post_leaderboard_score(data→height);
      skip;
    }
    bazaar→post_leaderboard_to_wall;
    wall→prompt("Ready?");
    code→main;
  }
  meta private;
}
#createboard
action create_board() {
  data→board := media→create_full_board;
  data→board→set_gravity(0, 1750);
  data→board→set_friction(0);
  data→board→set_background(colors→white);
  meta private;
}
#disrancemoved
var disrance_moved : Number {
}
#lastposition
var last_position : Number {
}
#newposition
var new_position : Number {
}
#createsafteynet
action create_saftey_net() {
  data→safety_net := data→board→create_rectangle(480, 200);
  data→safety_net→set_pos(240, 900);
  data→safety_net→set_friction(1);
  data→platforms→add(data→safety_net);
  meta private;
}
#safetynet
var safety_net : Sprite {
  transient = true;
}
#height
var height : Number {
}
#createscore
action create_score() {
  data→score := data→board→create_text(100, 20, 40, data→height→to_string);
  data→score→set_pos(240, 0);
  data→score→set_friction(1);
  data→score→set_color(colors→black);
  meta private;
}
#score
var score : Sprite {
  transient = true;
}
#gameended2
var game_ended2 : Boolean {
}
#doodlejump2
var doodle_jump2 : Picture {
  is\_resource = true;
  url = "http://profile.ak.fbcdn.net/hprofile-ak-snc4/50277_114607181899551_6226817_n.jpg";
  transient = true;
}
#platformimage
var platform_image : Picture {
  is\_resource = true;
  url = "http://www.clker.com/cliparts/l/z/Y/O/T/Q/wood-th.png";
  transient = true;
}
#picright
var picright : Picture {
  transient = true;
}
#picleft
var picleft : Picture {
  transient = true;
}
#bouncesound
var bounce_sound : Sound {
  is\_resource = true;
  url = "http://soundbible.com/grab.php?id=1343&type=wav";
  transient = true;
}