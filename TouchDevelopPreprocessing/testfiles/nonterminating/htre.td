meta version "v2.2,nothing";
meta name "a flock of seagulls";
meta icon "farm";
meta color "#ff007fff";
//
meta platform "current";

action main() {
  code→Init\u002EGraphics;
}
var bd : Board {
}

action Init\u002EGraphics() {
  $w := 480;
  $h := 800;
  data→pass := 0;
  data→bd := media→create_portrait_board($w, $h);
  data→bd→create_boundary(0);
  data→gulls := data→bd→create_sprite_set;
  for 0 ≤ i < 32 do {
    $gull := data→bd→create_ellipse(16, 16);
    if $i = 0 then {
      $gull→set_color(colors→accent);
      $vx, $vy := code→Speed(200);
      $gull→set_speed($vx, $vy);
    }
    else {
      $gull→set_color(colors→blue);
    }
    $gull→set_pos(math→random($w - 20) + 10, math→random($h - 20) + 10);
    if $gull→overlap_with(data→gulls)→count > 0 then {
      $gull→delete;
    }
    else {
      data→gulls→add($gull);
    }
  }
  data→bd→post_to_wall;
  meta private;
}
var gulls : Sprite_Set {
}

event gameloop() {
  code→loop2;
  meta private;
}
var pass : Number {
}

action Speed(base: Number) returns vx: Number, vy: Number {
  $angle := math→acos(math→random_normalized);
  $vx, $vy := code→SpeedEx($base, $angle);
  meta private;
}

action follow(g: Sprite, i: Number) {
  $mx := 120;
  data→gulls→at($i + 1)→speed_towards($g, $mx * 0.66 + math→random($mx * 0.33));
  meta private;
}
var lastpass : DateTime {
}

action SpeedEx(base: Number, angle: Number) returns vx: Number, vy: Number {
  $vy := $base * math→cos($angle);
  $vx := $base * math→sin($angle);
  meta private;
}

action loop2() {
  $mx := 120;
  $g := data→gulls→at(0);
  $crossover := code→Rand(100, 196);
  if math→mod(data→pass, $crossover) = 0 or $g→overlap_with(data→gulls)→count > 0 then {
    $vx, $vy := code→Speed($crossover);
    $g→set_speed($vx, $vy);
  }
  data→bd→evolve;
  data→turn := data→turn + 1;
  if math→mod(data→pass, 4) = 0 then {
    skip;
  }
  else {
    foreach gull in data→gulls where not $gull→equals($g) and $gull→overlap_with(data→gulls)→count > 0 do {
      code→Separate($gull);
    }
  }
  code→follow($g, math→mod(data→turn, data→gulls→count - 1));
  data→bd→evolve;
  data→pass := data→pass + 1;
  data→bd→update_on_wall;
  meta private;
}

action Rand(min: Number, max: Number) returns x: Number {
  $x := $min + math→random($max - $min);
  meta private;
}

event tap_board\u003A_bd(x: Number, y: Number) {
  data→gulls→at(0)→set_pos($x, $y);
  meta private;
}
var turn : Number {
}

action Separate(gull: Sprite) {
  $intersect := $gull→overlap_with(data→gulls);
  if not $intersect→contains($gull) and not $intersect→contains(data→gulls→at(0)) then {
    $gull→set_speed( - $gull→speed_x, - $gull→speed_y);
    $gull→set_pos(code→Rand(20, data→bd→width), code→Rand(20, data→bd→height));
    data→bd→evolve;
  }
  meta private;
}

