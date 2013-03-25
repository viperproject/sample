meta version "v2.2,nothing";
meta name "ReactionTime";
meta icon "alram";
meta color "#ff008000";
// Tap the screen when the color changes from black. The smaller result is the better.
meta platform "";

action main() {
  wall→clear;
  data→board := media→create_portrait_board(800, 600);
  data→board→set_gravity(0, 0);
  data→board→post_to_wall;
  data→ido := data→board→create_text(100, 20, 30, "Tap as fast as you can\nWhen the color changes");
  data→ido→set_pos(100, 100);
  data→debug := data→board→create_text(100, 20, 40, time→now→to_string);
  data→debug→hide;
  data→debug→set_pos(100, 200);
  data→lightning := time→now;
  data→lightning := data→lightning→add_seconds(math→random(5) + 5);
  data→debug→set_text(data→lightning→to_string);
  data→lightnin := false;
  data→img := data→board→create_picture(art→doakes);
  data→img→set_pos(225, 1000);
  data→img\_move := false;
  data→stop := false;
  data→board→update_on_wall;
}
var board : Board {
}
var ido : Sprite {
}

event gameloop() {
  if not data→stop then {
    if data→lightning→second = time→now→second and not data→lightnin then {
      data→board→set_background(colors→random);
      data→board→update_on_wall;
      data→tmp := (time→now→millisecond) + (1000 * time→now→second) + (60000 * time→now→minute);
      data→lightnin := true;
    }
    if data→img\_move then {
      data→img→set_y(data→img→y - 20);
      data→board→update_on_wall;
    }
  }
  meta private;
}
var lightning : DateTime {
}
var debug : Sprite {
}
var lightnin : Boolean {
}

event tap_board\u003A_board(x: Number, y: Number) {
  if not data→stop then {
    if data→lightnin then {
      $msec := (time→now→millisecond) + (1000 * time→now→second) + (60000 * time→now→minute) - data→tmp;
      data→ido→set_text($msec→to_string ∥ "ms");
      data→debug→set_text($msec→to_string);
      bazaar→post_leaderboard_score($msec);
      bazaar→post_leaderboard_to_wall;
      data→stop := true;
    }
    else {
      data→img→set_pos(225, 1000);
      data→ido→set_text("Wait for the color to change");
      data→lightning := time→now;
      data→lightning := data→lightning→add_seconds(math→random(5) + 5);
      data→img\_move := true;
    }
  }
  meta private;
}
var tmp : Number {
}
var doakes : Picture {
  is\_resource = true;
  url = "https://byfiles.storage.live.com/y1p2nfDLrUWBLA6bxh4azF5ZnNLQscFgKBwvo1ksJwr3unjJ-KUBf71nt9SguzR3gzVztWFAdB_MmupdqtBkkWnEw/27650421.jpg?psid=1";
}
var img : Sprite {
}
var img\_move : Boolean {
}
var stop : Boolean {
}

