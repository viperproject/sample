meta version "v2.2,nothing";
meta name "Tanks";
meta icon "video";
meta color "#ffff2800";
// Prototype tanks game. Leave suggestions and hearts please.
meta platform "current";

action main() {
  code→variables;
  code→menu;
  data→endd := false;
  data→b := false;
  data→jdns := 0;
  data→board→post_to_wall;
}
var board : Board {
}
var tank : Sprite {
}

event gameloop() {
  if data→b then {
    code→tankShoot;
    code→moveTank;
    code→tankAI;
    code→AITankShoot;
    code→hits;
    code→borders;
    data→board→evolve;
    data→board→update_on_wall;
  }
  meta private;
}
var missile : Picture {
  is\_resource = true;
  url = "http://www.clker.com/cliparts/2/w/b/p/t/C/missile-hi.png";
}

action tankShoot() {
  if data→board→touched and data→confirmFire and data→move then {
    $x := data→board→touch_start→x;
    $y := data→board→touch_start→y;
    data→target→set_pos($x, $y);
    data→pBullet := data→board→create_ellipse(15, 15);
    data→pBullet→set_color(colors→blue);
    data→pBullet→set_pos(data→tank→x, data→tank→y);
    data→pBullet→speed_towards(data→target, 150);
    data→playerBullets→add(data→pBullet);
    data→tankTarget→set_pos(math→random(400) + 50, math→random(700 + 50));
    data→confirmFire := false;
    data→AIbool := false;
  }
  meta private;
}
var AIbullet : Sprite {
}

action moveTank() {
  $rotateScale := .75;
  $p := senses→acceleration_smooth→scale(300);
  data→tank→set_speed($p→x, $p→y);
  data→tank→set_angle(data→tank→speed_x * $rotateScale);
  data→tank→set_angle(data→tank→speed_y * $rotateScale);
  meta private;
}
var target : Sprite {
}
var playerBullets : Sprite_Set {
}
var confirmFire : Boolean {
}
var AItank : Sprite {
}

action tankAI() {
  if data→move then {
    $x := 4.5;
    if data→AIbool then {
      data→AItank→speed_towards(data→tankTarget, 120);
      data→AItank→set_angular_speed(30);
    }
    if not data→AIbool then {
      data→AItank→speed_towards(data→tank, - 125);
      data→AItank→set_angular_speed( - 80);
      if((data→AItank→x < 40 or data→AItank→x > 440) and (data→AItank→y < 40 or data→AItank→y > 760)) then {
        data→tankTarget→set_pos(math→random(300) + 100, 800 - data→tank→y);
        data→AIbool := true;
      }
    }
    if math→ieee_remainder(data→AIshots, 200) = 0 and data→AIbool then {
      data→tankTarget→set_pos(math→random(400) + 50, math→random(700));
    }
    if data→AItank→overlaps_with(data→tankTarget) and data→AIbool then {
      data→tankTarget→set_pos(math→random(400) + 50, math→random(700) + 50);
    }
  }
  meta private;
}
var tankTarget : Sprite {
}
var AIbool : Boolean {
}

action AITankShoot() {
  if math→ieee_remainder(data→AIshots, 15) = 0 and data→tankshoot then {
    data→AIbullet := data→board→create_ellipse(15, 15);
    data→AIbullet→set_color(colors→red);
    data→AIbullet→set_pos(data→AItank→x, data→AItank→y);
    data→AIbullet→speed_towards(data→tank, 150);
    data→AITankBullets→add(data→AIbullet);
  }
  data→AIshots := data→AIshots + 1;
  meta private;
}
var AIshots : Number {
}
var x1 : Number {
}

action obstacles() {
  data→board→create_obstacle(data→board→width / 2 - 50, data→board→height / 2, 100, 1, 1);
  data→board→create_obstacle(100, 200, 100, 1, 1);
  data→board→create_obstacle(60, 500, 80, 1, 1);
  data→board→create_obstacle(200, 700, 100, 1, 1);
  data→board→create_obstacle(300, 150, 100, 1, 1);
  data→board→create_obstacle(300, 600, 80, 1, 1);
  meta private;
}
var AITankBullets : Sprite_Set {
}
var pBullet : Sprite {
}
var tankshoot : Boolean {
}
var x12 : Number {
}
var secondFire : Boolean {
}
var AITankBarrel : Sprite {
}
var barrelAnchor : Sprite {
}
var x : Number {
}

action hits() {
  foreach sprite in data→tank→overlap_with(data→AITankBullets) where true do {
    data→move := false;
    data→AItank→set_angular_speed(120);
    data→tank→delete;
    $sprite→delete;
    data→tankshoot := false;
    data→confirmFire := false;
    data→sprite32 := data→board→create_text(5, 20, 20, "Please post suggestions on cloud version. \nThis is just a beta.");
    data→sprite32→set_pos(65, 400);
    data→sprite22 := data→board→create_text(5, 20, 40, "You Lose!! \n");
    data→sprite22→set_pos(150, 200);
    ... ;
    data→endd := true;
  }
  foreach sprite1 in data→AItank→overlap_with(data→playerBullets) where true do {
    data→move := false;
    data→AItank→delete;
    $sprite1→delete;
    data→tankshoot := false;
    data→confirmFire := false;
    data→sprite4 := data→board→create_text(5, 20, 40, "You Win!! \n");
    data→sprite4→set_pos(150, 240);
    data→sprite32 := data→board→create_text(5, 20, 20, "Please post suggestions on cloud version. \nThis is just a beta.");
    data→sprite32→set_pos(65, 400);
    data→endd := true;
  }
  if data→endd then {
    data→jdns := data→jdns + 1;
  }
  if data→jdns = 75 then {
    data→endd := false;
    wall→clear;
    wall→prompt("Play again?");
    code→main;
  }
  meta private;
}

action borders() {
  foreach sprite1 in data→AITankBullets where true do {
    if $sprite1→x > 472 or $sprite1→x < 10 or $sprite1→y > 790 or $sprite1→y < 10 then {
      $sprite1→delete;
    }
  }
  foreach sprite in data→playerBullets where true do {
    if data→pBullet→x > 472 or data→pBullet→x < 10 or data→pBullet→y > 790 or data→pBullet→y < 10 then {
      data→confirmFire := true;
      data→AIbool := true;
      $sprite→delete;
    }
  }
  meta private;
}

action variables() {
  data→AIshots := 0;
  data→confirmFire := false;
  data→secondFire := true;
  data→AIbool := true;
  data→tankshoot := false;
  data→x12 := 0;
  data→start := false;
  meta private;
}

action items() {
  data→playerBullets := data→board→create_sprite_set;
  data→AITankBullets := data→board→create_sprite_set;
  data→tank := data→board→create_rectangle(50, 60);
  data→tank→set_pos(100, 750);
  data→tank→set_color(colors→blue);
  data→tank→set_friction(.2);
  data→target := data→board→create_ellipse(20, 20);
  data→target→set_opacity(0);
  data→AItank := data→board→create_rectangle(50, 60);
  data→AItank→set_pos(430, 50);
  data→AItank→set_color(colors→red);
  data→AItank→set_friction(.2);
  data→tankTarget := data→board→create_ellipse(200, 200);
  data→tankTarget→set_opacity(0);
  data→tankTarget→set_pos(math→random(400) + 20, 50);
  meta private;
}

action menu() {
  data→board := media→create_full_board;
  data→board→create_boundary(0);
  data→board→set_background(colors→black);
  data→sprite := data→board→create_rectangle(250, 80);
  data→sprite→set_pos(data→board→width / 2, data→board→height / 2 - 75);
  data→sprite→set_color(colors→dark_gray);
  data→sprite1 := data→board→create_rectangle(245, 75);
  data→sprite1→set_pos(data→board→width / 2, data→board→height / 2 - 75);
  data→sprite1→set_color(colors→red→darken(0.4));
  data→sprite2 := data→board→create_text(300, 50, 45, "Start Game");
  data→sprite2→set_color(colors→white);
  data→sprite2→set_pos(data→board→width / 2, data→board→height / 2 - 85);
  data→sprite3 := data→board→create_text(100, 20, 80, "Tanks");
  data→sprite3→set_pos(190, 100);
  meta private;
}
var start : Boolean {
}
var sprite : Sprite {
}
var sprite1 : Sprite {
}
var sprite2 : Sprite {
}
var sprite3 : Sprite {
}
var v : Number {
}

event tap_sprite\u003A_sprite2(sprite: Sprite, x: Number, y: Number) {
  code→animate;
  code→items;
  code→obstacles;
  data→sprite→delete;
  data→sprite1→delete;
  data→sprite2→delete;
  data→sprite3→delete;
  time→sleep(.25);
  data→diections := data→board→create_text(100, 20, 20, "Steer the tank by tilting your device.\nTap target on the screen to shoot.\nKill the other tank before it kills you!\nYou can only fire another shot once your\n last shot leaves the screen. \n\nPlease leave suggestions too (:");
  data→diections→set_pos(125, data→board→height / 2 - 100);
  data→board→update_on_wall;
  time→sleep(7);
  data→diections→delete;
  data→tankshoot := true;
  data→confirmFire := true;
  data→move := true;
  data→b := true;
  data→tankTarget→set_pos(100, 100);
  data→board→update_on_wall;
  meta private;
}

action animate() {
  data→sprite→set_width(230);
  data→sprite→set_height(70);
  data→sprite1→set_height(65);
  data→sprite1→set_width(225);
  data→board→update_on_wall;
  time→sleep(.2);
  data→sprite→set_width(240);
  data→sprite→set_height(80);
  data→sprite1→set_height(75);
  data→sprite1→set_width(235);
  data→board→update_on_wall;
  time→sleep(.1);
  meta private;
}
var move : Boolean {
}
var vector : Vector3 {
}
var b : Boolean {
}
var tutorial : Number {
}
var sprite22 : Sprite {
}
var sprite32 : Sprite {
}
var endd : Boolean {
}
var jdns : Number {
}
var diections : Sprite {
}
var sprite4 : Sprite {
}

