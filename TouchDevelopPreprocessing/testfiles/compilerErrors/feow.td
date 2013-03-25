meta version "v2.2,nothing";
meta name "deathsHead";
meta icon "BusinessPerson";
meta color "#ffff0038";
// Give your friends a fright. Tell them there is something funny about your phone.
//
// Take a photo with the camera icon or load a stored photo.
//
// Without them noticing, secretly tap the forehead of one or more people. Hand over the phone and after a few seconds a skull will fade over that selected face or faces!
meta platform "current";

action main() {
  code→init;
  $c := - 1;
  $menu := "Instructions:Resize image:Get picture" →split(":");
  while $c ≠ 2 do {
    wall;
    $c := wall→pick_string("", "", $menu);
    if $c = 0 then {
      code→instructions;
    }
    else {
      if $c = 1 then {
        code→resize;
      }
    }
    wall→clear;
  }
  code→getPicture;
}
var kept : Picture {
}

action init() {
  if records→retained_index→singleton→width→get→is_invalid or records→retained_index→singleton→width→get < 50 or records→retained_index→singleton→width→get > 300 then {
    records→retained_index→singleton→width→set(100);
  }
  data→skull2 := art→skull→clone;
  data→skull2→resize(records→retained_index→singleton→width→get, - 1);
  data→touched := false;
  data→touchedPosX := - 1;
  data→touchedPosY := - 1;
  data→opacity := .9;
  data→nPoints := 0;
  data→startTime := invalid→datetime;
  data→xPoints := collections→create_number_collection;
  data→yPoints := collections→create_number_collection;
  data→periods := collections→create_number_collection;
  data→delay := 4;
  data→minPeriod := 10;
  data→maxPeriod := 20;
  skip;
  meta private;
}

action fadeIn(point: Number) {
  $left := data→xPoints→at($point) - data→skull2→width / 2;
  $@top := data→yPoints→at($point) - data→skull2→height / 2;
  $elapsed := time→now→subtract(data→startTime) - data→delay;
  $amp := math→mod($elapsed * 1000 + data→periods→at($point) * 500, data→periods→at($point) * 1000);
  $amp := $amp / (data→periods→at($point) * 1000);
  $amp := math→abs($amp - 0.5) * data→opacity * 2;
  data→pic→blend(data→skull2, $left, $@top, 0, $amp);
  meta private;
}
var opacity : Number {
}
var skullWidth : Number {
}
var sprites : Sprite_Set {
}
var board : Board {
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
    data→pic := data→kept→clone;
    for 0 ≤ i < data→nPoints do {
      code→fadeIn($i);
    }
    data→sprite→set_picture(data→pic);
  }
  skip;
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
var skull : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/cgkklrnd";
}
var skull2 : Picture {
}

action resize() {
  $x := 0;
  $b := false;
  while not $b do {
    wall→clear;
    data→skull2→post_to_wall;
    $x2 := wall→ask_number("Enter number between 50 and 300 for image width (currently " ∥ records→retained_index→singleton→width→get→to_string ∥ ")");
    if $x2 > 49 and $x2 < 301 then {
      records→retained_index→singleton→width→set($x2);
      data→skull2 := art→skull→clone;
      data→skull2→resize(records→retained_index→singleton→width→get, - 1);
      wall→clear;
      data→skull2→post_to_wall;
      $b := wall→ask_boolean("Size OK?", "");
    }
  }
  wall→clear;
  meta private;
}

action instructions() {
  wall→prompt("Give your friends a fright. Tell them there is something funny about your phone.\n\nTake a photo with the camera icon or load a stored photo. \n\nWithout them noticing, secretly tap the forehead of one or more people. Hand over the phone and after a few seconds a skull will fade over that selected face or faces!\n\nYou can resize the skull before getting the picture if you want.\n\nNB If you do not have a good internet connection at the moment switch the phone into flight mode. Otherwise the app will get stuck looking for photos online.");
  meta private;
}
table retained {
  type = "Index";
  fields {
    width: Number_field
  }
}

action getPicture() {
  $pic := media→choose_picture;
  if $pic→is_invalid then {
    wall→clear;
    time→stop;
  }
  data→kept := $pic→clone;
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

