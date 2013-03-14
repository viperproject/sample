meta version "v2.2,nothing";
meta name "vampireDetector";
meta color "#ffff0038";
// Tell your friends you have an app to detect vampires! (Modern vampires can photograph OK.) Take a photo of a group or load a stored photo. Surreptiously tap the face of one or more people you want to be "detected". Hand over the phone and after a few seconds the selected face or faces will spookily start changing!
meta platform "current";

action main() {
  code→init;
}
var kept : Picture {
}

action init() {
  data→touched := false;
  data→touchedPosX := - 1;
  data→touchedPosY := - 1;
  data→wavelength := 30;
  data→amplitude := 10;
  data→rippleOffset := 0;
  data→rippleIncrement := 5;
  data→rippleWidth := 100;
  data→rippleHeight := 100;
  data→nPoints := 0;
  data→startTime := invalid→datetime;
  data→xPoints := collections→create_number_collection;
  data→yPoints := collections→create_number_collection;
  data→periods := collections→create_number_collection;
  data→delay := 4;
  data→minPeriod := 10;
  data→maxPeriod := 20;
  data→rippleRate := 5;
  data→kept := media→choose_picture→clone;
  if not data→kept→is_panorama then {
    data→kept→resize(480, - 1);
    data→board := media→create_portrait_board(480, 800);
  }
  else {
    data→kept→resize( - 1, 480);
    data→board := media→create_landscape_board(800, 480);
  }
  data→savedPic := data→kept→clone;
  data→pic := data→kept→clone;
  data→sprites := data→board→create_sprite_set;
  data→sprite := data→board→create_picture(data→pic);
  data→sprites→add(data→sprite);
  wall→clear;
  data→board→post_to_wall;
  meta private;
}

action ripple(point: Number) {
  $left := data→xPoints→at($point) - data→rippleWidth / 2;
  $@top := data→yPoints→at($point) - data→rippleHeight / 2;
  $elapsed := time→now→subtract(data→startTime);
  $theta := $elapsed / (data→periods→at($point) / data→rippleRate) * 2 * math→\u03C0;
  $amp := math→mod($elapsed * 1000, data→periods→at($point) * 1000);
  $amp := $amp / (data→periods→at($point) * 1000);
  $amp := math→abs($amp - 0.5) * data→amplitude * 2;
  for 0 ≤ i < data→rippleHeight do {
    $y := $@top + $i;
    if $y > - 1 and $y < data→pic→height then {
      $displacement := math→sin(($i + $theta) / data→wavelength * 2 * math→\u03C0) * $amp;
      for 0 ≤ j < data→rippleWidth do {
        $x := $left + $j;
        if $x > - 1 and $x < data→pic→width then {
          data→pic→set_pixel($x, $y, data→savedPic→pixel($x, $y - $displacement));
        }
      }
    }
  }
  meta private;
}
var wavelength : Number {
}
var amplitude : Number {
}
var rippleHeight : Number {
}
var rippleWidth : Number {
}
var sprites : Sprite_Set {
}
var board : Board {
}
var rippleIncrement : Number {
}
var rippleOffset : Number {
}
var pic : Picture {
}
var savedPic : Picture {
}

event gameloop() {
  if data→touched and data→startTime→is_invalid then {
    data→startTime := time→now;
  }
  if data→nPoints ≠ 0 and time→now→subtract(data→startTime) > data→delay then {
    for 0 ≤ i < data→nPoints do {
      code→ripple($i);
    }
    data→rippleOffset := data→rippleOffset + 1;
    data→sprite→set_picture(data→pic);
  }
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}
var touched : Boolean {
}
var touchedPosX : Number {
}
var touchedPosY : Number {
}

event tap_board\u003A_board(x: Number, y: Number) {
  data→touched := true;
  data→touchedPosX := $x;
  data→touchedPosY := $y;
  "Tapped" →post_to_wall;
  meta private;
}
var sprite : Sprite {
}

event tap_sprite_in_sprites(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if data→startTime→is_invalid or time→now→subtract(data→startTime) < data→delay then {
    data→touched := true;
    if not data→pic→is_panorama then {
      data→touchedPosX := $x;
      data→touchedPosY := $y - (data→board→height - data→pic→height) / 2;
    }
    else {
      data→touchedPosX := $x - (data→board→width - data→pic→width) / 2;
      data→touchedPosY := $y;
    }
    data→xPoints→add(data→touchedPosX);
    data→yPoints→add(data→touchedPosY);
    data→nPoints := data→nPoints + 1;
    data→periods→add(data→minPeriod + math→random_normalized * (data→maxPeriod - data→minPeriod));
  }
  meta private;
}
var nPoints : Number {
}
var startTime : DateTime {
}
var xPoints : Number_Collection {
}
var yPoints : Number_Collection {
}
var periods : Number_Collection {
}
var minPeriod : Number {
}
var maxPeriod : Number {
}
var delay : Number {
}
var rippleRate : Number {
}

