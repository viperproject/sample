meta version "v2.2,nothing";
meta name "BETA: Watch you fingers! for Phone";
meta icon "SmartPhone";
meta color "#ffFFA500";
// Tap the hole and tap it again when the blade falls. Watch you fingers! - When Microsoft releases the 'touch up' and 'touch down' events, you can touch the hole until the blade falls.
meta platform "current";

action main() {
  data→fall := false;
  data→board := media→create_portrait_board(480, 800);
  data→board→set_background(colors→sepia);
  data→background := data→board→create_rectangle(440, 580);
  data→background→set_color(colors→blue);
  data→background→set_pos(240, 310);
  data→hole := data→board→create_ellipse(100, 100);
  data→hole→set_color(colors→green);
  data→hole→set_pos(240, 700);
  data→blade := data→board→create_rectangle(440, 100);
  data→blade→set_color(colors→light_gray);
  data→blade→set_pos(240, 60);
  data→board→post_to_wall;
  data→loops := 0;
  data→looptext := data→board→create_text(100, 20, 40, data→loops ∥ "");
  data→looptext→set_color(colors→black);
  data→looptext→set_pos(130, 200);
  data→score := 0;
  data→scoretext := data→board→create_text(100, 20, 40, data→loops ∥ "");
  data→scoretext→set_color(colors→black);
  data→scoretext→set_pos(350, 200);
}
var accel : Number {
}
var background : Sprite {
}
var board : Board {
}
var fall : Boolean {
}
var hole : Sprite {
}
var blade : Sprite {
}
var loops : Number {
}
var looptext : Sprite {
}
var score : Number {
}
var scoretext : Sprite {
}
var start : Number {
}
var touch : Boolean {
}

event gameloop() {
  data→accel := 5000;
  if data→touch→equals(true) then {
    data→loops := data→loops + 1;
    data→looptext→delete;
    data→looptext := data→board→create_text(100, 20, 40, data→loops ∥ "");
    data→looptext→set_color(colors→black);
    data→looptext→set_pos(130, 200);
    if math→random(250) = 1 then {
      data→fall := true;
      data→blade→set_acceleration_y(5000);
      data→start := data→loops;
    }
  }
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}

event tap_sprite\u003A_hole(sprite: Sprite, x: Number, y: Number) {
  if data→touch→equals(false) then {
    data→touch := true;
    data→hole→set_color(colors→black);
  }
  else {
    if data→fall→equals(true) then {
      data→background→set_color(colors→green);
      if data→blade→y > 650 then {
        data→score := - 1;
      }
      else {
        data→score := data→score + (data→accel / (data→loops - data→start));
      }
      data→scoretext→delete;
      data→scoretext := data→board→create_text(100, 20, 40, data→score ∥ "");
      data→scoretext→set_pos(350, 200);
      data→scoretext→set_color(colors→black);
      data→blade→set_pos(240, 60);
      data→blade→set_acceleration_y(0);
      data→blade→set_speed_y(0);
      data→fall := false;
      data→background→set_color(colors→blue);
    }
    data→touch := false;
    data→loops := 0;
    data→hole→set_color(colors→green);
  }
  meta private;
}

