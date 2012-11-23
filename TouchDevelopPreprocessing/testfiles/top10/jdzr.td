meta version "v2.2,nothing";
meta name "Rouages";
meta icon "settings";
meta color "#ffa1caf1";
// I say here that it's useless
meta platform "current";
var bd : Board {
}

action rouage() {
  //
  for 0 ≤ i < 24 do {
    records→thing_index→at($i)→restants→set($i);
    data→Roues→at($i)→hide;
  }
  $sens := 1;
  records→thing_index→at(0)→angles→set(0);
  records→thing_index→at(0)→rapport→set(1);
  $n1 := math→rand(24);
  $ray1 := records→thing_index→at($n1)→rayons→get;
  records→thing_index→at(0)→ordre→set($n1);
  records→thing_index→at($n1)→sortis→set(1);
  code→sortis(24);
  $sp := data→Roues→at($n1);
  $ray2 := 0;
  $ray := 0;
  $ang := 0;
  $angSp := 0;
  $nDent1 := $n1 + 7;
  $x := 240;
  $y := 400;
  $sp→set_pos($x, $y);
  $sp→show;
  records→thing_index→at(0)→x→set($x);
  records→thing_index→at(0)→y→set($y);
  //
  $nSp := 23;
  $sort := 1;
  $n2 := 0;
  while $sort < 15 do {
    $sens := $sens * - 1;
    if $sort < 13 then {
      $n2 := math→rand($nSp);
      records→thing_index→at($n2)→sortis→set(1);
      $n2 := records→thing_index→at($n2)→restants→get;
      code→sortis($nSp);
      $nSp := $nSp - 1;
    }
    else {
      if $sort = 13 then {
        $n2 := records→thing_index→at(1)→restants→get;
      }
      else {
        $n2 := records→thing_index→at(0)→restants→get;
      }
    }
    $nDent2 := $n2 + 7;
    $x1 := records→thing_index→at($sort - 1)→rapport→get * ($nDent1 / $nDent2);
    records→thing_index→at($sort)→rapport→set($x1);
    records→thing_index→at($sort)→ordre→set($n2);
    $ray2 := records→thing_index→at($n2)→rayons→get;
    $ray := $ray1 + $ray2;
    $ang := (360 / $nDent1) * math→rand($nDent1) + $angSp;
    $x := $x + $ray * math→cos($ang * data→pi);
    $y := $y + $ray * math→sin($ang * data→pi);
    records→thing_index→at($sort)→x→set($x);
    records→thing_index→at($sort)→y→set($y);
    $sp := data→Roues→at($n2);
    $sp→set_pos($x, $y);
    // Décalage = 10% d'une dent
    if $sens = - 1 then {
      $angSp := ($ang + 180) + (72 / $nDent2);
    }
    else {
      $angSp := ($ang + 180);
    }
    $angSp := code→setAngSp($angSp);
    $sp→set_angle($angSp);
    $sp→show;
    records→thing_index→at($sort)→angles→set($angSp);
    $n1 := $n2;
    $nDent1 := $n1 + 7;
    $ray1 := $ray2;
    $sort := $sort + 1;
  }
  code→setVeil();
  $sens := - 1;
  for 0 ≤ i2 < 15 do {
    $sens := $sens * - 1;
    records→thing_index→at($i2)→rapport→set(records→thing_index→at($i2)→rapport→get * $sens);
    data→ombre→at(records→thing_index→at($i2)→ordre→get)→set_pos(records→thing_index→at($i2)→x→get, records→thing_index→at($i2)→y→get);
  }
  meta private;
}
var Roues : Sprite_Set {
}

action sortis(n: Number) {
  $px := 0;
  for 0 ≤ i < $n + 1 do {
    if records→thing_index→at($i)→sortis→get = 0 then {
      records→thing_index→at($px)→restants→set(records→thing_index→at($i)→restants→get);
      $px := $px + 1;
    }
    else {
      records→thing_index→at($i)→sortis→set(0);
    }
  }
  meta private;
}

action acceptColor() returns select: Number {
  data→pic0→draw_text(5, 650, "Accept color", 40, 0, colors→foreground);
  data→pic0→draw_text(320, 650, "Change", 40, 0, colors→foreground);
  data→bd→update_on_wall;
  $select := 0;
  while $select = 0 do {
    while data→bd→touched→equals(false) do {
      ... ;
    }
    $p := data→bd→touch_start;
    if $p→y > 650 then {
      if $p→x < 240 then {
        $select := 1;
      }
      else {
        $select := 2;
      }
    }
  }
  meta private;
}

action decalage(p: Vector3) {
  $deltaX := ($p→x - 240) / 10;
  $deltaY := ($p→y - 400) / 10;
  while data→bd→touched do {
    for 0 ≤ i < 15 do {
      $sp := data→Roues→at(records→thing_index→at($i)→ordre→get);
      $x := records→thing_index→at($i)→x→get - $deltaX;
      $y := records→thing_index→at($i)→y→get - $deltaY;
      $sp→set_pos($x, $y);
      records→thing_index→at($i)→x→set($x);
      records→thing_index→at($i)→y→set($y);
    }
    $p := data→bd→touch_current;
    $deltaX := ($p→x - 240) / 10;
    $deltaY := ($p→y - 400) / 10;
    data→bd→update_on_wall;
  }
  time→sleep(.3);
  ... ;
  meta private;
}
var spText : Sprite {
}

action pin_to_start_or_reinitialize() {
  // Reiniti
  data→bd := invalid→board;
  code→not_pin_to_start;
}

action setVeil() {
  data→veil→delete;
  data→veil := data→bd→create_picture(data→pic0);
  meta private;
}
var spCom : Sprite_Set {
}

action vitesse(p: Vector3) {
  while data→bd→touched do {
    if $p→x > 70 then {
      data→pas := data→pas + .1;
    }
    else {
      if data→pas > .1 then {
        data→pas := data→pas - .1;
      }
    }
    data→spCom→at(1)→set_text(math→round(data→pas * 10) / 10 ∥ "");
    data→bd→update_on_wall;
    time→sleep(.1);
  }
  meta private;
}

action dimension(p: Vector3) {
  //
  if false then {
    $ii := 0;
    $size := 1;
    $touch := 0;
    while $touch = 0 do {
      if $p→x > 200 then {
        $size := 1.05;
      }
      else {
        $size := .95;
      }
      while data→bd→touched do {
        for 0 ≤ i < 24 do {
          records→thing_index→at($i)→rayons→set(records→thing_index→at($i)→rayons→get * $size);
          //
          data→Roues→at($i)→set_width(data→Roues→at($i)→width * $size);
          data→Roues→at($i)→set_height(data→Roues→at($i)→height * $size);
          //
          if $i < 12 then {
            $x := 240 + (records→thing_index→at($i)→x→get - 240) * $size;
            records→thing_index→at($i)→x→set($x);
            $y := 400 + (records→thing_index→at($i)→y→get - 400) * $size;
            records→thing_index→at($i)→y→set($y);
            $n1 := records→thing_index→at($i)→ordre→get;
            data→Roues→at($n1)→set_pos(records→thing_index→at($i)→x→get, records→thing_index→at($i)→y→get);
          }
        }
        data→bd→update_on_wall;
      }
      $p := data→bd→touch_start;
      if $p→y < 700 or $p→x < 130 or $p→x > 270 then {
        $touch := 1;
      }
    }
  }
  meta private;
}

action commandes(p: Vector3) {
  while data→bd→touched→equals(false) do {
    skip;
  }
  $p := data→bd→touch_start;
  if $p→x > 270 then {
    data→touch := 1;
  }
  else {
    if $p→x > 130 then {
      // A bug report :  code-> dimension
      code→dimension2($p);
    }
    else {
      code→vitesse($p);
    }
  }
  meta private;
}
var touch : Number {
}
var pas : Number {
}

action roues24() {
  records→picRoues_index→clear;
  data→ombre := data→bd→create_sprite_set;
  $roue := 0;
  while $roue < 24 do {
    data→c := colors→from_hsb((data→c→hue) + .004, 1, 1)→make_transparent(0.6);
    code→drawRoue(7 + $roue, 0);
    $sp := data→bd→create_picture(data→pic);
    data→Roues→add($sp);
    $roue := $roue + 1;
    data→bd→update_on_wall;
  }
  meta private;
}

event gameloop() {
  data→touch := 2;
  while data→touch ≠ 0 do {
    data→ix := 0;
    while data→ix < 15 do {
      $sp := data→Roues→at(records→thing_index→at(data→ix)→ordre→get);
      //
      $angleSp := records→thing_index→at(data→ix)→angles→get + data→pas * records→thing_index→at(data→ix)→rapport→get;
      if $angleSp > 360 then {
        $angleSp := $angleSp - 360;
      }
      else {
        if $angleSp < 0 then {
          $angleSp := $angleSp + 360;
        }
      }
      $sp→set_angle($angleSp);
      records→thing_index→at(data→ix)→angles→set($angleSp);
      //
      data→ix := data→ix + 1;
    }
    data→bd→update_on_wall;
    if data→bd→touched then {
      code→bdTouched;
      if data→touch = 1 then {
        code→rouage;
        data→touch := 0;
        data→bd→update_on_wall;
        while data→bd→touched do {
          skip;
        }
      }
    }
  }
  meta private;
}

action initSetSpCom() {
  data→spCom := data→bd→create_sprite_set;
  $pic := media→create_picture(120, 60);
  $pic→fill_ellipse(0, 0, 120, 60, 0, colors→gray);
  $pic→draw_ellipse(0, 0, 120, 60, 0, colors→linear_gradient(colors→gray, colors→brown, .5), 3);
  $pic→fill_rect(50, 0, 20, 60, 0, colors→gray);
  $pic→draw_text(15, 15, "-", 25, 0, colors→black);
  $pic→draw_text(92, 13, "+", 30, 0, colors→black);
  $pic1 := $pic→clone;
  $pic→draw_text(31, 2, "Speed", 20, 0, colors→black);
  $sp := data→bd→create_picture($pic);
  $sp→set_pos(70, 750);
  $sp→hide;
  // 0 vitesse
  data→spCom→add($sp);
  $sp := data→bd→create_text(40, 20, 40, "1");
  $sp→set_color(colors→black);
  $sp→set_pos(70, 738);
  $sp→hide;
  // 1 affichage
  data→spCom→add($sp);
  $pic := media→create_picture(120, 60);
  $pic→fill_ellipse(0, 0, 120, 60, 0, colors→gray);
  $pic→draw_ellipse(0, 0, 120, 60, 0, colors→linear_gradient(colors→gray, colors→brown, .5), 3);
  $pic→draw_text(22, 18, "Random", 20, 0, colors→black);
  $sp := data→bd→create_picture($pic);
  $sp→set_pos(330, 750);
  $sp→hide;
  // 2 random
  data→spCom→add($sp);
  $pic1→draw_text(36, 2, "Size", 20, 0, colors→black);
  $sp := data→bd→create_picture($pic1);
  $sp→hide;
  $sp→set_pos(200, 750);
  // 3 dimension
  data→spCom→add($sp);
  meta private;
}

action roues0() {
  $n := math→random_normalized;
  data→c := colors→from_hsb($n, 1, 1)→make_transparent(0.6);
  code→drawRoue(7, 1);
  data→pic0→clear(colors→background);
  data→pic0→blend(data→pic, 200, 365, 0, 1);
  $select := code→acceptColor();
  if $select = 2 then {
    data→bd→update_on_wall;
    code→roues0;
  }
  else {
    data→pic0 := media→create_picture(480, 800);
    data→pic0→draw_line(0, 700, 480, 700, colors→linear_gradient(colors→black, colors→orange, .8), 2);
    data→bd→set_background_picture(data→pic0);
    data→bd→update_on_wall;
  }
  meta private;
}
var pic0 : Picture {
}
var pi : Number {
}

action drawRoue(nDent: Number, first: Number) {
  $circonf := $nDent * 16;
  $rayon := math→round($circonf / math→\u03C0);
  $ray := $rayon + 4;
  records→thing_index→at($nDent - 7)→rayons→set($ray);
  $ray2 := $ray + 4;
  data→pic := media→create_picture($ray2 * 2, $ray2 * 2);
  if data→drawS = 1 then {
    code→drawText($rayon);
  }
  $angle := 360 / $nDent;
  $iteration := 0;
  while $iteration < $nDent do {
    $ang := ($angle * $iteration) * data→pi;
    $x := $ray2 + $ray * math→cos($ang);
    $y := $ray2 + $ray * math→sin($ang);
    $ang := ($angle * .4 + ($angle * $iteration)) * data→pi;
    $x1 := $ray2 + $ray * math→cos($ang);
    $y1 := $ray2 + $ray * math→sin($ang);
    data→pic→draw_line($x, $y, $x1, $y1, data→c, 8);
    $iteration := $iteration + 1;
  }
  data→pic→draw_ellipse($ray2 - $rayon, $ray2 - $rayon, $rayon * 2, $rayon * 2, 0, data→c, 3);
  records→picRoues_index→at($nDent - 7)→pic→set(data→pic);
  records→thing_index→at($nDent - 7)→w→set($ray2 * 2);
  $sp1 := data→bd→create_ellipse($ray2, $ray2);
  $sp1→set_color(data→c);
  $sp1→hide;
  if $first = 0 then {
    data→ombre→add($sp1);
  }
  meta private;
}
var c : Color {
}
var pic : Picture {
}
var rot : Number {
}
var veil : Sprite {
}

action bdTouched() {
  $p := data→bd→touch_start;
  if $p→y < 700 then {
    if data→comOn = 0 then {
      code→decalage($p);
    }
    else {
      code→endCom;
    }
  }
  else {
    if data→comOn = 0 then {
      data→comOn := 1;
      for 0 ≤ i < 4 do {
        data→spCom→at($i)→show;
      }
      data→bd→update_on_wall;
      while data→bd→touched do {
        skip;
      }
    }
    else {
      code→commandes($p);
    }
  }
  meta private;
}

action dimension2(p: Vector3) {
  $ii := 0;
  $size := 1;
  for 0 ≤ i < 15 do {
    $n := records→thing_index→at($i)→ordre→get;
    data→Roues→at($n)→hide;
    data→ombre→at($n)→set_pos(records→thing_index→at($i)→x→get, records→thing_index→at($i)→y→get);
    data→ombre→at($n)→show;
    data→bd→update_on_wall;
  }
  //
  data→etalon := 1;
  if $p→x > 200 then {
    $size := 1.05;
  }
  else {
    $size := .95;
  }
  while data→bd→touched do {
    data→etalon := data→etalon * $size;
    if records→thing_index→at(0)→w→get * data→etalon > 1 then {
      for 0 ≤ i4 < 24 do {
        data→ombre→at($i4)→set_width(data→ombre→at($i4)→width * $size);
        data→ombre→at($i4)→set_height(data→ombre→at($i4)→height * $size);
        if $i4 < 15 then {
          $x := 240 + (records→thing_index→at($i4)→x→get - 240) * $size;
          records→thing_index→at($i4)→x→set($x);
          $y := 400 + (records→thing_index→at($i4)→y→get - 400) * $size;
          records→thing_index→at($i4)→y→set($y);
          data→ombre→at(records→thing_index→at($i4)→ordre→get)→set_pos($x, $y);
        }
      }
      data→bd→update_on_wall;
    }
    else {
      data→etalon := data→etalon / $size;
      $size := 1;
    }
  }
  //
  for 0 ≤ i3 < 4 do {
    data→spCom→at($i3)→hide;
  }
  data→bd→update_on_wall;
  code→redraw;
  for 0 ≤ i2 < 15 do {
    $n1 := records→thing_index→at($i2)→ordre→get;
    data→Roues→at($n1)→show;
    data→Roues→at($n1)→set_pos(records→thing_index→at($i2)→x→get, records→thing_index→at($i2)→y→get);
    data→ombre→at($n1)→hide;
    data→bd→update_on_wall;
  }
  for 0 ≤ i1 < 4 do {
    data→spCom→at($i1)→show;
  }
  data→bd→update_on_wall;
  meta private;
}
var bug : Sprite {
}
table picRoues {
  type = "Index";
  keys {
    n: Number
  }
  fields {
    pic: Picture_field
    width: Number_field
  }
}
var ombre : Sprite_Set {
}

action redraw() {
  data→veil→delete;
  for 0 ≤ i < 24 do {
    records→thing_index→at($i)→rayons→set(records→thing_index→at($i)→rayons→get * data→etalon);
    $w := records→thing_index→at($i)→w→get;
    $w := $w * data→etalon;
    records→thing_index→at($i)→w→set($w);
    $pic := records→picRoues_index→at($i)→pic→get→clone;
    if $w < 1 then {
      $w := 1;
    }
    $pic→resize($w, - 1);
    $sprite1 := data→Roues→at(0);
    $sprite1→delete;
    data→Roues→remove_first;
    $sprite1 := data→bd→create_picture($pic);
    $sprite1→hide;
    data→Roues→add($sprite1);
  }
  code→setVeil;
  data→bd→update_on_wall;
  meta private;
}
var etalon : Number {
}

action drawText(rayon: Number) {
  $centre := $rayon + 8;
  for 0 ≤ i < 12 do {
    $ang := $i * 30;
    $x := $centre + ($rayon * .9) * math→cos($ang * data→pi);
    $y := $centre + ($rayon * .9) * math→sin($ang * data→pi);
    data→pic→draw_text($x, $y, data→s→substring($i, 1), $rayon / 4, (98 + $ang), data→c);
  }
  meta private;
}
var s : String {
}
var ix : Number {
}
var comOn : Number {
}

action endCom() {
  data→comOn := 0;
  for 0 ≤ i2 < 4 do {
    data→spCom→at($i2)→hide;
  }
  data→bd→update_on_wall;
  meta private;
}

action not_pin_to_start() {
  // If some problems with pin to start and this script a old script to solve delete main tile (data pin to start tile) unpin and pin again
  if data→bd→is_invalid then {
    data→bd := media→create_full_board;
    data→pas := 1;
    data→comOn := 0;
    data→touch := 0;
    code→initSetSpCom;
    data→pic0 := media→create_picture(480, 800);
    data→bd→post_to_wall;
    code→intro;
    data→pi := math→\u03C0 / 180;
    data→s := "RAISONNEMENT";
    code→selectDrawS;
    code→roues0;
    data→Roues := data→bd→create_sprite_set;
    code→roues24;
  }
  else {
    data→bd→post_to_wall;
  }
  code→rouage;
}

action intro() {
  data→pic0→draw_text(10, 180, "Touch the screen to move the wheels\n in the direction from touched point\n       to the center of the screen\n at a speed accorded to the distance\n  between touched point and center\n     Tap under the line for a menu\n        Tap screen to hide menu\n ( Press longer than a tap on random\n so much to do with all these wheels )\n                             tap to start", 27, 0, colors→foreground);
  data→bd→set_background_picture(data→pic0);
  data→bd→update_on_wall;
  while data→bd→touched→equals(false) do {
    skip;
  }
  data→pic0→clear(colors→background);
  data→bd→update_on_wall;
  meta private;
}
table thing {
  type = "Index";
  keys {
    n: Number
  }
  fields {
    angles: Number_field
    rapport: Number_field
    ordre: Number_field
    x: Number_field
    y: Number_field
    sortis: Number_field
    restants: Number_field
    rayons: Number_field
    w: Number_field
  }
}

action setAngSp(angSp: Number) returns x: Number {
  if $angSp > 360 then {
    $angSp := $angSp - 360;
  }
  if $angSp > 360 then {
    $angSp := $angSp - 360;
  }
  $x := $angSp;
  meta private;
}

action selectDrawS() {
  data→pic0→draw_text(5, 650, "    Text \non wheels", 40, 0, colors→foreground);
  data→pic0→draw_text(300, 650, " No text \non wheels", 40, 0, colors→foreground);
  data→bd→update_on_wall;
  data→drawS := 0;
  while data→drawS = 0 do {
    while data→bd→touched→equals(false) do {
      ... ;
    }
    $p := data→bd→touch_start;
    if $p→y > 650 then {
      if $p→x < 240 then {
        data→drawS := 1;
      }
      else {
        data→drawS := 2;
      }
    }
  }
  data→pic0→clear(colors→background);
  data→bd→update_on_wall;
  meta private;
}
var drawS : Number {
}

