meta version "v2.2,nothing";
meta name "Fun Food Picker";
// Tap the screen to creates bubbles!
meta platform "current";
meta import HUD {
  pub "ztne"
  usage {
    action initialize(board: Board)
    action set_score(value: Number)
    action set_life(value: Number)
    action start_timer()
    action evolve()
  }
}
meta import splash_screen {
  pub "aoiq"
  usage {
  }
  resolve animations = ♻animations with {
  }
  resolve easing = ♻easing}
meta import animations {
  pub "vaej"
  usage {
  }
  resolve easing = ♻easing with {
  }
}
meta import easing {
  pub "fxjo"
  usage {
  }
}

action main() {
  // The board is a 2D canvas with a physics engine. Gravity makes sprites fall and boundary makes them bounce.
  data→score := 0;
  wall→set_background(colors→white);
  skip;
  data→board := media→create_landscape_board(800, 480);
  data→board→set_background(colors→random);
  data→sprite_set := data→board→create_sprite_set;
  data→board→post_to_wall;
}
var score : Number {
}

event gameloop() {
  // Apply physics and redraw.
  data→board→evolve;
  ♻ HUD→evolve;
  ♻ HUD→set_score(data→score);
  if true then {
    $selected := math→random(data→sprite_set→count);
    if math→random(100) > 90 then {
      if data→sprite_set→at($selected)→is_visible then {
        data→sprite_set→at($selected)→hide;
      }
      else {
        data→sprite_set→at($selected)→show;
      }
    }
  }
  data→board→update_on_wall;
  meta private;
}
var sprite_set : Sprite_Set {
}
var board : Board {
}
var Good_Food_Orange : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/nmkndkqo";
}
var Bad_Food_Donut : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/qskxmmot";
}

action Score_Display_Initialize() {
  ♻ HUD→initialize(data→board);
  ♻ HUD→set_life(3);
  ♻ HUD→set_score(data→score);
  ♻ HUD→start_timer;
  meta private;
}

event tap_sprite_in_sprite_set(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $sprite→is_visible then {
    art→bad_sound_burp→play;
    data→score := data→score + 1;
    $sprite→hide;
  }
  meta private;
}
var Bad_Food_Hamburger : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/jsybhqxs";
}
var Good_Food_Apple : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/bzjjeplq";
}
var Good_Sound_Trill : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/xrnrpomq";
}
var bad_sound_burp : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/otvkwmcb";
}

event tap_board\u003A_board(x: Number, y: Number) {
  code→Score_Display_Initialize;
  code→GamePlay;
  meta private;
}

action GamePlay() {
  for 0 ≤ j < 2 do {
    for 0 ≤ i < 3 do {
      $donuts := data→board→create_picture(art→Bad_Food_Donut);
      $donuts→set_width(100);
      $donuts→set_pos(100 + $i * 200, 100 + $j * 200);
      $donuts→hide;
      skip;
      data→sprite_set→add($donuts);
    }
  }
  meta private;
}

