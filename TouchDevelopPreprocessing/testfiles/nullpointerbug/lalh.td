meta version "v2.2,nothing";
meta name "Musiquads";
meta icon "threecolumn";
meta color "#ffff2800";
// Tap screen to add blocks, swipe to change background and add a group of blocks, tap black block to play/change music, tap grey to replay song.
meta showAd "yes";
meta platform "current";

action main() {
  data→fsmode := false;
  data→board := media→create_portrait_board(480, 800);
  data→board→set_background(colors→black);
  data→board→create_boundary(0);
  data→sprite := data→board→create_rectangle(20, 20);
  data→sprite→set_color(colors→black);
  data→prev := data→board→create_rectangle(120, 60);
  data→prev→set_pos(0, 0);
  data→prev→set_color(colors→subtle);
  data→next := data→board→create_rectangle(120, 60);
  data→next→set_pos(data→board→width - data→next→width, 0);
  data→next→set_color(colors→chrome);
  data→board→post_to_wall;
}
var board : Board {
}
var sprite : Sprite {
}

action Physics() {
  data→epilepsymode := true;
  $a := senses→acceleration_quick→scale(500);
  data→board→set_gravity($a→x, $a→y);
  data→board→evolve;
  data→board→update_on_wall;
  data→sprite→set_color(colors→random);
  if data→fsmode→equals(true) then {
    data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
    data→\u002E\u002E\u002E\u002E→set_color(colors→random);
    skip;
  }
}

event gameloop() {
  code→Physics;
  meta private;
}

action Supersquare() {
  data;
  data→Supersquare2 := data→board→create_rectangle(40, 40);
  data→Supersquare2→move_towards(data→sprite, 10);
  ... ;
  meta private;
}
var Supersquare2 : Sprite {
}
var \u002E\u002E\u002E\u002E : Sprite {
}

event tap_board\u003A_board(x: Number, y: Number) {
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}
var epilepsymode : Boolean {
}

event swipe_board\u003A_board(x: Number, y: Number, delta_x: Number, delta_y: Number) {
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→\u002E\u002E\u002E\u002E := data→board→create_rectangle(math→random(40), math→random(40));
  data→\u002E\u002E\u002E\u002E→set_color(colors→random);
  data→board→set_background(colors→random);
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}
var maxblocks : Number {
}

event active_song_changed() {
  data→maxblocks = data→maxblocks + 5;
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}
var prev : Sprite {
}
var fsmode : Boolean {
}
var next : Sprite {
}

event tap_sprite\u003A_prev(sprite: Sprite, x: Number, y: Number) {
  player→previous;
  meta private;
}

event tap_sprite\u003A_next(sprite: Sprite, x: Number, y: Number) {
  media→songs→random→play;
  meta private;
}

