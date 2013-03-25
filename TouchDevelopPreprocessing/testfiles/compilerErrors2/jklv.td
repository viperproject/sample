meta version "v2.2,nothing";
meta name "Submarine";
meta icon "joystick";
meta color "#ff008080";
// You are Captain of a submarine in enemy waters. Using only your sonar, you must deploy countermeasures to detonate enemy torpedoes before they strike your hull. If you receive too much damage, your ship and your crew will be lost!
// Awesome script by Sore Finger. Added a small improvement that shows the enemies only when radar swipes it. Check it out.
meta platform "";
meta import serif {
  pub "dzdba"
  usage {
    action from(sans: String) returns serif: String
    action to(serif: String) returns sans: String
  }
}

action main() {
  data→points := 0;
  data→game_over := false;
  data→hull := 100;
  data→visibleAngle := 90;
  wall→set_background(colors→black);
  wall→set_foreground(colors→green);
  wall→set_reversed(true);
  code→create_board;
  code→draw_line;
  data→overlay := data→board→create_sprite_set;
  $sprite := data→board→create_rectangle(480, 800);
  $sprite→set_color(colors→transparent);
  data→overlay→add($sprite);
  code→draw_overlay;
  code→increment_enemy_timer;
  data→board→post_to_wall;
}

event gameloop() {
  if data→game_over then {
    wall→clear;
    bazaar→post_leaderboard_score(♻ serif→to(data→points_display→text)→to_number);
    ♻ serif→from("Game Over")→post_to_wall;
    bazaar→post_leaderboard_to_wall;
    while true do {
      data→board→clear_events;
    }
  }
  else {
    code→create_enemy;
    code→update_line;
    code→update_touch_lines;
    code→update_enemies;
    $b := false;
    while data→remove→count > 0 do {
      data→remove→at(0)→delete;
      data→remove→remove_first;
      $b := true;
    }
    if $b then {
      wall→clear;
      data→board→post_to_wall;
    }
    data→board→evolve;
    data→board→update_on_wall;
  }
  meta private;
}

action draw_line_sprite() returns sprite: Sprite {
  $sprite := data→board→create_rectangle(3, 467);
  $sprite→set_pos(240, 166);
  records→Sprite_decorator→at($sprite)→x→set($sprite→x);
  records→Sprite_decorator→at($sprite)→y→set($sprite→y);
  data→line→add($sprite);
  meta private;
}

action update_offset_rotation(sprite: Sprite, angle: Number, offsetX: Number, offsetY: Number) {
  $angle := math→mod($sprite→angle + $angle, 360);
  $sprite→set_angle($angle);
  $angle := math→deg_to_rad($angle);
  $a := math→create_vector3(math→cos($angle), math→sin($angle), 0)→scale( - $offsetX);
  $b := math→create_vector3( - math→sin($angle), math→cos($angle), 0)→scale( - $offsetY);
  $x := records→Sprite_decorator→at($sprite)→x→get + $offsetX;
  $y := records→Sprite_decorator→at($sprite)→y→get + $offsetY;
  $c := math→create_vector3($x, $y, 1);
  $p1 := $a→add($b→add($c));
  $sprite→set_pos($p1→x, $p1→y);
  foreach e in data→enemies where true do {
    $deltay := $e→y - data→board→height / 2;
    $deltax := $e→x - data→board→width / 2;
    $x2 := math→atan2($deltax, - $deltay);
    if $x2 < 0 then {
      $x2 := $x2 + 2 * math→\u03C0;
    }
    $x3 := ($angle - $x2) / math→\u03C0 * 180;
    if $x3 < 0 then {
      $x3 := $x3 + 360;
    }
    if $x3 > 0 and $x3 < data→visibleAngle then {
      $e→show;
      $e→set_opacity(1 - $x3 / data→visibleAngle);
    }
    else {
      $e→hide;
    }
    // show also if we are really close
    $distSq := $deltay * $deltay + $deltax * $deltax;
    if $distSq < 1000 then {
      $e→show;
      $e→set_opacity(1 - $distSq / 1000);
    }
  }
  meta private;
}
var board : Board {
}
table thing {
  type = "Decorator";
  keys {
    target: Sprite
  }
  fields {
    x: Number_field
    y: Number_field
  }
}
var line : Sprite_Set {
}

action create_board() {
  records→Sprite_decorator→clear;
  data→board := media→create_full_board;
  data→board→set_background(colors→green→make_transparent(0.05));
  data→line := data→board→create_sprite_set;
  data→enemies := data→board→create_sprite_set;
  data→touch_lines := data→board→create_sprite_set;
  data→remove := data→board→create_sprite_set;
  $vert := data→board→create_rectangle(1, 800);
  $vert→set_color(colors→green);
  $horiz := data→board→create_rectangle(480, 1);
  $horiz→set_color(colors→green);
  data→center := data→board→create_rectangle(7, 7);
  data→center→set_color(colors→green);
  $pl := data→board→create_text(1, 30, 20, ♻ serif→from("Points: "));
  $pl→set_pos(6, 740);
  $pl→set_color(colors→red);
  data→points_display := data→board→create_text(1, 30, 20, ♻ serif→from("0"));
  data→points_display→set_color(colors→red);
  data→points_display→set_pos(160, 740);
  $hl := data→board→create_text(1, 30, 20, ♻ serif→from("Hull   Integrity: "));
  $hl→set_pos(6, 770);
  $hl→set_color(colors→red);
  data→hull_display := data→board→create_rectangle(150, 15);
  data→hull_display→set_pos(394, 770);
  data→hull_display→set_color(colors→green);
  meta private;
}
var enemy_timer : DateTime {
}

action update_line() {
  foreach sprite in data→line where true do {
    code→update_offset_rotation($sprite, 4, 0, 234);
  }
  meta private;
}

action update_enemies() {
  foreach sprite in data→enemies where $sprite→is_visible and $sprite→overlaps_with(data→center) do {
    $sprite→hide;
    data→remove→add($sprite);
    $x := math→create_vector3($sprite→speed_x, $sprite→speed_y, 0)→length / 2;
    if data→hull_display→width > $x then {
      data→hull_display→set_width(data→hull_display→width - $x);
      data→hull_display→set_x(data→hull_display→x - $x / 2);
    }
    else {
      data→game_over := true;
    }
    data→hull_display→set_color(colors→from_hsb(data→hull_display→width / 540, 1, 1));
  }
  meta private;
}

action draw_line() {
  $x := 24;
  for 0 ≤ i < $x do {
    $sprite := code→draw_line_sprite();
    $sprite→set_color(colors→green→make_transparent($i * $i * 0.00026));
    code→update_offset_rotation($sprite, $i * 0.5, 0, 234);
  }
  data→line→at($x - 1)→set_color(colors→green);
  meta private;
}

action create_enemy() {
  if time→now→greater(data→enemy_timer) then {
    $sprite := data→board→create_text(25, 25, 25, "▲");
    $i := math→random(4);
    $x := math→random(480);
    $y := math→random(800);
    if $i = 0 then {
      $sprite→set_pos($x, 0);
    }
    else {
      if $i = 1 then {
        $sprite→set_pos(480, $y);
      }
      else {
        if $i = 2 then {
          $sprite→set_pos($x, 800);
        }
        else {
          $sprite→set_pos(0, $y);
        }
      }
    }
    $sprite→speed_towards(data→center, math→random(10) * code→multiplier + code→multiplier);
    $sprite→set_color(colors→green);
    data→enemies→add($sprite);
    code→draw_overlay;
    code→increment_enemy_timer;
  }
  meta private;
}

action increment_enemy_timer() {
  data→enemy_timer := time→now→add_seconds(code→multiplier);
  meta private;
}
var center : Sprite {
}
var enemies : Sprite_Set {
}
var overlay : Sprite_Set {
  // undefinedundefinedundefinedundefined
}

event tap_sprite_in_overlay(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  for 0 ≤ i < 3 do {
    $sprite1 := data→board→create_rectangle(1, 1);
    $sprite1→set_color(colors→red);
    $sprite1→set_pos($x, $y);
    $sprite1→set_angle(60 * $i);
    records→Sprite_decorator→at($sprite1)→x→set($x);
    records→Sprite_decorator→at($sprite1)→y→set($y);
    if $i = 0 then {
      $sprite1→move(0, 1);
    }
    else {
      $x1 := 0.5 * math→sqrt(3);
      $y1 := - 0.5;
      if $i = 1 then {
        $sprite1→move($x1, $y1);
      }
      else {
        $sprite1→move( - $x1, $y1);
      }
    }
    $speed := math→create_vector3($sprite1→x - $x, $sprite1→y - $y, 0)→scale(40);
    $sprite1→set_speed($speed→x, $speed→y);
    data→touch_lines→add($sprite1);
    code→draw_overlay;
  }
  meta private;
}

action draw_overlay() {
  // make sure overlay is on top
  $sprite := data→overlay→at(0);
  $sprite→set_z_index(500);
  meta private;
}
var touch_lines : Sprite_Set {
}

action update_touch_lines() {
  foreach sprite in data→touch_lines where true do {
    $x := records→Sprite_decorator→at($sprite)→x→get;
    $y := records→Sprite_decorator→at($sprite)→y→get;
    $distance := math→create_vector3($sprite→x, $sprite→y, 0)→distance(math→create_vector3($x, $y, 0)) + 1;
    $sprite→set_width($distance * math→\u03C0);
    if $distance < 76 then {
      $sprite→set_opacity(1 - ($distance / 75));
      if $distance < 15 then {
        code→hit_enemies($sprite);
      }
    }
    else {
      data→remove→add($sprite);
    }
  }
  foreach sprite1 in data→remove where data→touch_lines→contains($sprite1) do {
    data→touch_lines→remove($sprite1);
  }
  meta private;
}
var remove : Sprite_Set {
}

action hit_enemies(touch: Sprite) {
  $touch;
  foreach sprite in $touch→overlap_with(data→enemies) where true do {
    $x := math→create_vector3($sprite→speed_x, $sprite→speed_y, 0)→length;
    data→points := data→points + 1;
    $sprite→hide;
    $sprite→set_speed(0, 0);
    data→remove→add($sprite);
    data→points_display→set_text(♻ serif→from((math→round($x) * 10 + ♻ serif→to(data→points_display→text)→to_number)→to_string));
  }
  foreach sprite1 in data→remove where data→enemies→contains($sprite1) do {
    data→enemies→remove($sprite1);
  }
  meta private;
}
var points : Number {
}

action multiplier() returns x: Number {
  $p := data→points + 75;
  $x := math→log(8, $p) * 700 / $p;
  meta private;
}
var hull : Number {
  // undefinedundefinedundefinedundefined
}
var hull_display : Sprite {
  // undefinedundefinedundefinedundefined
}
var points_display : Sprite {
  // undefinedundefinedundefinedundefinedundefinedundefinedundefinedundefinedundefinedundefinedundefinedundefined
}
var game_over : Boolean {
  // undefinedundefinedundefinedundefined
}
var visibleAngle : Number {
  // undefinedundefinedundefinedundefined
}

