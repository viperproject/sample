meta version "v2.2,nothing";
meta name "explodePhoto";
meta icon "Camera";
// Tap a photo to make bits explode.
meta platform "current";

action main() {
  code→init;
  code→getPhoto;
  skip;
  wall→clear;
  data→board→post_to_wall;
}

action init() {
  data→exploding := false;
  data→explosionTime := 2;
  data→noise := 0.3;
  data→radius := 50;
  data→grid := 5;
  data→size := 10;
  data→k := 10;
  data→depth := 800;
  data→tooClose := 100;
  data→es := records→exploding_sprites→create;
  meta private;
}

event gameloop() {
  if data→exploding then {
    if time→now→subtract(data→explosionStartTime) < data→explosionTime then {
      data→board→evolve;
      code→updateExplosion(data→es);
      data→board→update_on_wall;
    }
    else {
      data→exploding := false;
      code→stopExplosion(data→es);
      data→board→update_on_wall;
    }
  }
  meta private;
}
var board : Board {
}
var tooClose : Number {
}
var debug : Sprite {
}
table exploding_sprites {
  type = "Object";
  fields {
    s: Sprite_Set_field
    zSpeed: Number_Collection_field
    projectedSize: Number_Collection_field
    startX: Number_field
    startY: Number_field
    depth: Number_field
    spriteSize: Number_field
  }
}

action updateExplosion(es: exploding_sprites) {
  data→board→evolve;
  $first := $es→s→get→at(0);
  $dSquared := ($first→x - $es→startX→get) * ($first→x - $es→startX→get) + ($first→y - $es→startY→get) * ($first→y - $es→startY→get);
  $sSquared := $first→speed_x * $first→speed_x + $first→speed_y * $first→speed_y;
  $t := math→sqrt($dSquared) / math→sqrt($sSquared);
  for 0 ≤ i < $es→s→get→count do {
    $z := $es→zSpeed→get→at($i) * $t;
    // this is not quite right because it ignores the original value z
    $d := $es→depth→get - $z;
    if $d > data→tooClose then {
      $pSize := math→round($es→spriteSize→get * $es→depth→get / $d);
      if $pSize ≠ $es→projectedSize→get→at($i) then {
        $es→projectedSize→get→set_at($i, $pSize);
        $es→s→get→at($i)→set_width($pSize);
        $es→s→get→at($i)→set_height($pSize);
        $es→s→get→at($i)→set_color($es→s→get→at($i)→color);
      }
    }
    else {
      $es→s→get→at($i)→hide;
    }
    skip;
  }
  data→board→update_on_wall;
  meta private;
}

action initSphere(es: exploding_sprites, x: Number, y: Number, depth: Number, radius: Number, gridSize: Number, spriteSize: Number, k: Number, noise: Number, pic: Picture, board: Board) {
  $es→spriteSize→set($spriteSize);
  $es→depth→set($depth);
  if $es→zSpeed→get→is_invalid then {
    $es→zSpeed→set(collections→create_number_collection);
  }
  while $es→zSpeed→get→count > 0 do {
    $es→zSpeed→get→remove_at(0);
  }
  if $es→projectedSize→get→is_invalid then {
    $es→projectedSize→set(collections→create_number_collection);
  }
  while $es→projectedSize→get→count > 0 do {
    $es→projectedSize→get→remove_at(0);
  }
  if $es→s→get→is_invalid then {
    $es→s→set($board→create_sprite_set);
  }
  while $es→s→get→count > 0 do {
    $es→s→get→remove_first;
  }
  $l := $x - $radius;
  $t := $y - $radius;
  $first := true;
  for 0 ≤ i < $radius * 2 / $gridSize do {
    for 0 ≤ j < $radius * 2 / $gridSize do {
      $ii := $i * $gridSize - $radius;
      $jj := $j * $gridSize - $radius;
      $zSquared := $radius * $radius - $ii * $ii - $jj * $jj;
      if $zSquared > 0 then {
        $sprite := $board→create_rectangle(200, 200);
        $sprite→set_pos($x + $ii, $y + $jj);
        $sprite→set_color($pic→pixel($ii + $radius, $jj + $radius));
        $z := math→sqrt($zSquared);
        $es→zSpeed→get→add($k * $z);
        $sprite→set_speed($k * $ii / $gridSize * (1 + (math→random_normalized - 0.5) * $noise), $k * $jj / $gridSize * (1 + (math→random_normalized - 0.5) * $noise));
        if $first then {
          $es→startX→set($x + $ii);
          $es→startY→set($y + $jj);
          $first := false;
        }
        $d := $es→depth→get - $z;
        $pSize := math→round($es→spriteSize→get * $es→depth→get / $d);
        $es→projectedSize→get→add($pSize);
        $sprite→set_width($pSize);
        $sprite→set_height($pSize);
        $es→s→get→add($sprite);
      }
    }
  }
  meta private;
}
var es : exploding_sprites {
}

action getPhoto() {
  data→photo := media→choose_picture;
  if data→photo→is_invalid then {
    time→stop;
  }
  data→photo := data→photo→clone;
  if data→photo→is_panorama then {
    data→board := media→create_landscape_board(800, 480);
    data→photo→resize( - 1, 480);
    $sprite := data→board→create_picture(data→photo);
    $sprite→set_pos(400, 240);
    data→sprites := data→board→create_sprite_set;
    data→sprites→add($sprite);
  }
  else {
    data→board := media→create_portrait_board(480, 800);
    data→photo→resize(480, - 1);
    $sprite2 := data→board→create_picture(data→photo);
    $sprite2→set_pos(240, 400);
    data→sprites := data→board→create_sprite_set;
    data→sprites→add($sprite2);
  }
  meta private;
}
var exploding : Boolean {
}
var explosionTime : Number {
}

action startExplosion(x: Number, y: Number) {
  $pic := data→photo→clone;
  $pic→crop($x - (data→board→width - data→photo→width) / 2 - data→radius, $y - (data→board→height - data→photo→height) / 2 - data→radius, data→radius * 2, data→radius * 2);
  code→initSphere(data→es, $x, $y, data→depth, data→radius, data→grid, data→size, data→k, data→noise, $pic, data→board);
  data→explosionStartTime := time→now;
  data→exploding := true;
  meta private;
}
var photo : Picture {
}
var noise : Number {
}
var radius : Number {
}
var grid : Number {
}
var size : Number {
}
var k : Number {
}
var depth : Number {
}
var explosionStartTime : DateTime {
}
var sprites : Sprite_Set {
}

event tap_sprite_in_sprites(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if not data→exploding then {
    code→startExplosion($x, $y);
  }
  meta private;
}

action stopExplosion(es: exploding_sprites) {
  while $es→zSpeed→get→count > 0 do {
    $es→zSpeed→get→remove_at(0);
  }
  while $es→projectedSize→get→count > 0 do {
    $es→projectedSize→get→remove_at(0);
  }
  foreach e in $es→s→get where true do {
    $e→hide;
  }
  $es→s→get→clear;
  meta private;
}

