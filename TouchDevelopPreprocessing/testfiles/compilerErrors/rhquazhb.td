meta version "v2.2,nothing";
meta name "explodingPhotoFromLibrary";
meta icon "Camera";
// This is a library to keep track of exploding spheres made from Pics.
meta platform "current";
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
    timeStarted: DateTime_field
    duration: Number_field
  }
}

action updateExplosion(es: exploding_sprites) {
  // need to surround this with board->evolve and board->update on wall
  $first := $es→s→get→at(0);
  $dSquared := ($first→x - $es→startX→get) * ($first→x - $es→startX→get) + ($first→y - $es→startY→get) * ($first→y - $es→startY→get);
  $sSquared := $first→speed_x * $first→speed_x + $first→speed_y * $first→speed_y;
  $t := math→sqrt($dSquared) / math→sqrt($sSquared);
  for 0 ≤ i < $es→s→get→count do {
    $z := $es→zSpeed→get→at($i) * $t;
    // this is not quite right because it ignores the original value z
    $d := $es→depth→get - $z;
    if $d > data→esTooClose then {
      $pSize := math→round($es→spriteSize→get * $es→depth→get / $d);
      if $pSize ≠ $es→projectedSize→get→at($i) then {
        $es→projectedSize→get→set_at($i, $pSize);
        $es→s→get→at($i)→set_width($pSize);
        $es→s→get→at($i)→set_height($pSize);
      }
    }
    else {
      $es→s→get→at($i)→hide;
    }
    skip;
  }
  meta private;
}

action initExplodingSphere(x: Number, y: Number, depth: Number, radius: Number, gridSize: Number, spriteSize: Number, k: Number, noise: Number, duration: Number, pic: Picture, board: Board) {
  if data→esCollection→is_invalid then {
    data→esCollection := records→exploding_sprites→create_collection;
  }
  $es := records→exploding_sprites→create;
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
        $sprite→set_speed($k * ($ii + (math→random_normalized - 0.5) * $noise) / $gridSize * (1 + (math→random_normalized - 0.5) * $noise), $k * ($jj + (math→random_normalized - 0.5) * $noise) / $gridSize * (1 + (math→random_normalized - 0.5) * $noise));
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
  $es→timeStarted→set(time→now);
  $es→duration→set($duration);
  data→esCollection→add($es);
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
    $es→s→get→remove($e);
  }
  meta private;
}
var esCollection : exploding_sprites_Collection {
}

action updateAllExplosions() {
  if data→esCollection→is_invalid then {
    data→esCollection := records→exploding_sprites→create_collection;
  }
  foreach e in data→esCollection where true do {
    if time→now→subtract($e→timeStarted→get) > $e→duration→get then {
      code→stopExplosion($e);
      data→esCollection→remove($e);
    }
    else {
      code→updateExplosion($e);
    }
  }
}

action setTooClose(p: Number) {
  data→esTooClose := $p;
}
var esTooClose : Number {
}

action main() {
  "This is main" →post_to_wall;
  wall→prompt("");
}

