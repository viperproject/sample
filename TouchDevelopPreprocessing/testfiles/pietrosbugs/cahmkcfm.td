meta version "v2.2,nothing";
meta name "Jeu de la vie webapp";
meta icon "grapes";
meta color "#ff004b49";
//
meta platform "current";

action La_vie() {
  data→Mere := math→create_number_map;
  $d := 20;
  $test := 0;
  $Yne := 0;
  $Xne := 0;
  $MereNes := 0;
  code→quad();
  data→bd→post_to_wall;
  meta private;
}

action Entrees() {
  $p := data→bd→touch_start;
  $d := data→pas2 * .75;
  $pic := data→pic;
  $x := data→pas2 / 2;
  $xx := math→round(($p→x - (400 - $x)) / data→pas2);
  $yy := math→round(($p→y - (240 - $x)) / data→pas2);
  $xp := 400 + ($xx * data→pas2) - $x;
  $yp := 240 + ($yy * data→pas2) - $x;
  $xx := $xx + data→X;
  $yy := $yy + data→Y;
  $xy := $xx * 1000 + $yy;
  if $pic→pixel($xp, $yp)→equals(colors→orange) then {
    data→Vie→set_at($xy, 1);
    data→Rang→set_at(data→Nnes, $xy);
    $pic→fill_ellipse($xp - $d / 2, $yp - $d / 2, $d, $d, 0, colors→magenta);
    data→Nnes := data→Nnes + 1;
    data→bd→update_on_wall;
  }
  else {
    data→Vie→set_at($xy, 0);
    $pic→fill_ellipse($xp - $d / 2, $yp - $d / 2, $d, $d, 0, colors→black);
    $pic→blend(data→pixel, $xp - 1, $yp - 1, 0, 1);
    $NeOk := 0;
    for 0 ≤ i < data→Nnes do {
      if data→Vie→at(data→Rang→at($i)) = 1 then {
        data→Rang→set_at($NeOk, data→Rang→at($i));
        $NeOk := $NeOk + 1;
      }
    }
    data→Nnes := data→Nnes - 1;
  }
  data→bd→set_background_picture($pic);
  data→bd→update_on_wall;
  time→sleep(.2);
  meta private;
}

action Figure() {
  $entree := 0;
  $p := data→bd→touch_start;
  while $entree = 0 do {
    while data→bd→touched do {
      skip;
    }
    while data→bd→touched→equals(false) do {
      skip;
    }
    data→touch := - 1;
    data→spPos→set_pos(data→bd→touch_start→x, data→bd→touch_start→y);
    for 0 ≤ j < 5 do {
      if data→spPos→overlaps_with(data→setCom1→at($j)) then {
        data→touch := $j;
      }
    }
    if data→touch ≠ - 1 then {
      if data→touch < 2 then {
        while data→bd→touched do {
          if data→touch = 0 then {
            if data→pas2 > 8 then {
              data→pas2 := data→pas2 - 2;
            }
          }
          else {
            data→pas2 := data→pas2 + 2;
          }
          code→quad();
          code→tracell;
          time→sleep(.02);
        }
      }
      else {
        if data→touch = 2 then {
          code→clear;
          data→pas2 := 30;
          code→quad();
        }
        else {
          if data→touch = 3 then {
            if data→spPos→x < data→setCom1→at(3)→x then {
              // Rand
              code→clear;
            }
            code→rand;
            code→quad();
            code→tracell;
          }
          else {
            $entree := 1;
            if data→pas > data→pas2 then {
              data→pas := data→pas2;
            }
            for 0 ≤ i2 < 5 do {
              data→setCom1→at($i2)→hide;
            }
            data→Ngen := 0;
          }
        }
      }
    }
    else {
      code→Entrees;
    }
  }
  data→Nnew := data→Nnes;
  for 0 ≤ i1 < data→Nnes do {
    data→Vie→set_at(data→Rang→at($i1), 1);
  }
  data→pic→clear(colors→black);
  data→pic2→clear(colors→black);
  meta private;
}

action naissent(x: Number, y: Number, xy: Number) {
  // Test des cases voisines d'une case vide
  $Ii := $xy;
  $Ne := 0;
  $xy := ($x - 1) * 1000 + $y - 1;
  if data→Vie→at($xy) ≠ 0 then {
    $Ne := $Ne + 1;
  }
  $xy := $x * 1000 + $y - 1;
  if data→Vie→at($xy) ≠ 0 then {
    $Ne := $Ne + 1;
  }
  $xy := ($x + 1) * 1000 + $y - 1;
  if data→Vie→at($xy) ≠ 0 then {
    $Ne := $Ne + 1;
  }
  $xy := ($x - 1) * 1000 + $y;
  if data→Vie→at($xy) ≠ 0 then {
    $Ne := $Ne + 1;
  }
  $xy := ($x + 1) * 1000 + $y;
  if data→Vie→at($xy) ≠ 0 then {
    $Ne := $Ne + 1;
  }
  $xy := ($x - 1) * 1000 + $y + 1;
  if data→Vie→at($xy) ≠ 0 then {
    $Ne := $Ne + 1;
  }
  $xy := $x * 1000 + $y + 1;
  if data→Vie→at($xy) ≠ 0 then {
    $Ne := $Ne + 1;
  }
  $xy := ($x + 1) * 1000 + $y + 1;
  if data→Vie→at($xy) ≠ 0 then {
    $Ne := $Ne + 1;
  }
  data→Nait→set_at($Ii, $Ne);
  if $Ne = 3 then {
    data→Nait→set_at(data→Nnew, $Ii);
    data→Nnew := data→Nnew + 1;
  }
  data→Vues→set_at(data→vu, $Ii);
  data→vu := data→vu + 1;
  meta private;
}

action vivent() {
  // Nombre de voisins et test cases vides voisines
  data→Nnew := 0;
  $vie := 0;
  $xy := 0;
  $x1 := 0;
  $x := 0;
  $y := 0;
  $Ii := 0;
  for 0 ≤ i2 < data→Nnes do {
    $vie := 1;
    $Ii := data→Rang→at($i2);
    $x := math→floor($Ii / 1000);
    $y := $Ii - $x * 1000;
    $xy := ($x - 1) * 1000 + $y - 1;
    if data→Vie→at($xy) ≠ 0 then {
      // Si cette case voisine est occupée
      $vie := $vie + 1;
    }
    else {
      // Si la case voisine vide n'a pas été testée
      if data→Nait→at($xy) = 0 then {
        code→naissent($x - 1, $y - 1, $xy);
      }
    }
    $xy := $x * 1000 + $y - 1;
    if data→Vie→at($xy) ≠ 0 then {
      $vie := $vie + 1;
    }
    else {
      if data→Nait→at($xy) = 0 then {
        code→naissent($x, $y - 1, $xy);
      }
    }
    $xy := ($x + 1) * 1000 + $y - 1;
    if data→Vie→at($xy) ≠ 0 then {
      $vie := $vie + 1;
    }
    else {
      if data→Nait→at($xy) = 0 then {
        code→naissent($x + 1, $y - 1, $xy);
      }
    }
    $xy := ($x - 1) * 1000 + $y;
    if data→Vie→at($xy) ≠ 0 then {
      $vie := $vie + 1;
    }
    else {
      if data→Nait→at($xy) = 0 then {
        code→naissent($x - 1, $y, $xy);
      }
    }
    $xy := ($x + 1) * 1000 + $y;
    if data→Vie→at($xy) ≠ 0 then {
      $vie := $vie + 1;
    }
    else {
      if data→Nait→at($xy) = 0 then {
        code→naissent($x + 1, $y, $xy);
      }
    }
    $xy := ($x - 1) * 1000 + $y + 1;
    if data→Vie→at($xy) ≠ 0 then {
      $vie := $vie + 1;
    }
    else {
      if data→Nait→at($xy) = 0 then {
        code→naissent($x - 1, $y + 1, $xy);
      }
    }
    $xy := $x * 1000 + $y + 1;
    if data→Vie→at($xy) ≠ 0 then {
      $vie := $vie + 1;
    }
    else {
      if data→Nait→at($xy) = 0 then {
        code→naissent($x, $y + 1, $xy);
      }
    }
    $xy := ($x + 1) * 1000 + $y + 1;
    if data→Vie→at($xy) ≠ 0 then {
      $vie := $vie + 1;
    }
    else {
      if data→Nait→at($xy) = 0 then {
        code→naissent($x + 1, $y + 1, $xy);
      }
    }
    data→Vie→set_at($Ii, $vie);
    if data→Vie→at($Ii) = 3 or data→Vie→at($Ii) = 4 then {
      data→Nait→set_at(data→Nnew, $Ii);
      data→Nnew := data→Nnew + 1;
    }
    else {
      data→Vie→set_at($Ii, - 1);
    }
  }
  for 0 ≤ i1 < data→vu do {
    data→Nait→set_at(data→Vues→at($i1), 0);
  }
  data→vu := 0;
  meta private;
}

action quad() {
  $pas := data→pas2;
  $pic := data→pic;
  $pic→clear(colors→black);
  $nx := 1 + math→floor(400 / $pas);
  $ny := 1 + math→floor(240 / $pas);
  $x := 399 - $pas / 2;
  $y := 239 - $pas / 2;
  for 0 ≤ ix < $nx do {
    for 0 ≤ iy < $ny do {
      $pic→blend(data→pixel, ($x - $ix * $pas), ($y - $iy * $pas), 0, 1);
      $pic→blend(data→pixel, $x + $ix * $pas, $y - $iy * $pas, 0, 1);
      $pic→blend(data→pixel, $x - $ix * $pas, $y + $iy * $pas, 0, 1);
      $pic→blend(data→pixel, $x + $ix * $pas, $y + $iy * $pas, 0, 1);
    }
  }
  data→bd→update_on_wall;
  meta private;
}

action tracell() {
  skip;
  $pic := data→pic;
  $rayon := data→pas2 * 1.5;
  $xp := 0;
  $yp := 0;
  $x := 400 - data→pas2 / 2;
  $y := 240 - data→pas2 / 2;
  for 0 ≤ i1 < data→Nnes do {
    $xp := math→floor(data→Rang→at($i1) / 1000);
    $yp := (((data→Rang→at($i1) - $xp * 1000)) - data→Y) * data→pas2;
    $xp := ($xp - data→X) * data→pas2;
    $pic→fill_ellipse($x + $xp - $rayon / 4, $y + $yp - $rayon / 4, $rayon / 2, $rayon / 2, 0, colors→magenta);
  }
  data→bd→update_on_wall;
  meta private;
}
var fr : Number {
}

action decal() {
  $xDec := 0;
  $yDec := 0;
  if data→pions2→at(0)→is_visible then {
    $xDec := data→pions2→at(0)→x;
    $yDec := data→pions2→at(0)→y;
  }
  else {
    $xDec := data→pions4→at(0)→x;
    $yDec := data→pions4→at(0)→y;
  }
  while data→bd→touched do {
    $x := math→floor((data→bd→touch_current→x - 400)) / 10;
    $y := math→floor((data→bd→touch_current→y - 240)) / 10;
    code→reTrace( - $x, - $y);
    time→sleep(.02);
  }
  if data→pions2→at(0)→is_visible then {
    $xDec := $xDec - data→pions2→at(0)→x;
    $yDec := $yDec - data→pions2→at(0)→y;
  }
  else {
    $xDec := $xDec - data→pions4→at(0)→x;
    $yDec := $yDec - data→pions4→at(0)→y;
  }
  data→X := data→X + math→floor($xDec / data→pas);
  data→Y := data→Y + math→floor($yDec / data→pas);
  data→bd→update_on_wall;
  meta private;
}

action decal2() {
  $spr := data→setCom1→at(3);
  $spr→set_pos(240, 400);
  $b := false;
  while data→bd→touched→equals(false) do {
    while $b→equals(false) do {
      $spr→hide;
      data→bd→update_on_wall;
      $x1 := 5000;
      while $b→equals(false) and $x1 > 0 do {
        $b := data→bd→touched;
        $x1 := $x1 - 1;
      }
      $spr→show;
      data→bd→update_on_wall;
      $x1 := 5000;
      while $b→equals(false) and $x1 > 0 do {
        $b := data→bd→touched;
        $x1 := $x1 - 1;
      }
    }
  }
  $p := data→bd→touch_start;
  while data→bd→touched do {
    skip;
  }
  $spr→show;
  $spr→set_pos(310, 720);
  $p1 := data→bd→touch_end;
  $x := math→floor(($p→x - 240) / data→pas2);
  $y := math→floor(($p→y - 400) / data→pas2);
  data→X := data→X - (math→floor(($p1→x - 240) / data→pas2) - $x);
  data→Y := data→Y - (math→floor(($p1→y - 400) / data→pas2) - $y);
  meta private;
}

action trace() {
  $nSp0 := 0;
  $nSp2 := 0;
  $nSp4 := 0;
  $x := 0;
  $y := 0;
  $x2 := 0;
  $y2 := 0;
  $dg := 0;
  data→Npar→set_at(data→Ngen, data→Nnew);
  if data→boucle = 0 then {
    for 0 ≤ i2 < data→Nnew do {
      $x := math→floor(data→Nait→at($i2) / 1000);
      $y := 240 + (((data→Nait→at($i2) - $x * 1000)) - data→Y) * data→pas;
      $x := 400 + ($x - data→X) * data→pas;
      if data→Vie→at(data→Nait→at($i2)) < 3 then {
        data→pions2→at($nSp2)→set_pos($x, $y);
        data→pions2→at($nSp2)→show;
        $nSp2 := $nSp2 + 1;
        $dg := 2000000;
      }
      else {
        data→pions4→at($nSp4)→set_pos($x, $y);
        data→pions4→at($nSp4)→show;
        $nSp4 := $nSp4 + 1;
        $dg := 4000000;
      }
      data→Gene→set_at($i2 + data→Ngen * 1000, $dg + data→Nait→at($i2));
    }
  }
  data→Ngen := data→Ngen + 1;
  if data→Tsp4 > $nSp4 then {
    for 0 ≤ i5 < data→Tsp4 - ($nSp4 - 1) do {
      data→pions4→at($nSp4 + $i5)→hide;
    }
  }
  if data→Tsp2 > $nSp2 then {
    for 0 ≤ i6 < data→Tsp2 - ($nSp2 - 1) do {
      data→pions2→at($nSp2 + $i6)→hide;
    }
  }
  data→Tsp2 := $nSp2;
  data→Tsp4 := $nSp4;
  if data→traceSp0 = 1 then {
    for 0 ≤ i1 < data→Nnes do {
      if data→Vie→at(data→Rang→at($i1)) < 0 then {
        $x2 := math→floor(data→Rang→at($i1) / 1000);
        $y2 := (((data→Rang→at($i1) - $x2 * 1000)) - data→Y) * data→pas;
        $x2 := ($x2 - data→X) * data→pas;
        data→pions0→at($nSp0)→set_pos(400 + $x2, 240 + $y2);
        data→pions0→at($nSp0)→show;
        $nSp0 := $nSp0 + 1;
      }
      data→Vie→set_at(data→Rang→at($i1), 0);
    }
    if data→Tsp0 > $nSp0 then {
      for 0 ≤ i8 < data→Tsp0 - ($nSp0 - 1) do {
        data→pions0→at($nSp0 + $i8)→hide;
      }
    }
    data→Tsp0 := $nSp0;
  }
  else {
    for 0 ≤ i < data→Nnes do {
      data→Vie→set_at(data→Rang→at($i), 0);
    }
  }
  meta private;
}
var Tsp0 : Number {
}
var Tsp2 : Number {
}
var Tsp4 : Number {
}

action initCom1() {
  // Dessin commandes figure
  $y := 450;
  $c := colors→magenta→darken(0.4);
  data→setCom1 := data→bd→create_sprite_set;
  // 0 >..<
  $pic := media→create_picture(90, 50);
  $pic2 := $pic→clone;
  $pic3 := $pic→clone;
  $pic→fill_ellipse(0, 0, 90, 50, 0, colors→dark_gray);
  $pic→draw_ellipse(2, 2, 86, 46, 0, $c, 3);
  $pic5 := $pic→clone;
  $pic→draw_text(12, 5, ">• ", 35, 0, colors→black);
  $pic→draw_text(44, 5, "•<", 35, 0, colors→black);
  $sp := data→bd→create_picture($pic);
  $sp→set_pos(90, $y);
  data→setCom1→add($sp);
  // 1 <. .>
  $pic2→fill_ellipse(0, 0, 90, 50, 0, colors→light_gray);
  $pic2→draw_ellipse(2, 2, 86, 46, 0, $c, 3);
  $pic6 := $pic2→clone;
  $pic2→draw_text(9, 5, "<•", 35, 0, colors→black);
  $pic2→draw_text(39, 5, " •>", 35, 0, colors→black);
  $sp := data→bd→create_picture($pic2);
  $sp→set_pos(245, $y);
  data→setCom1→add($sp);
  // Mire
  $pic4 := media→create_picture(40, 40);
  $pic4→draw_text(9, 4, "O", 30, 0, colors→foreground);
  $pic4→draw_line(20, 0, 20, 40, colors→white, 1);
  $pic4→draw_line(0, 21, 40, 21, colors→white, 1);
  if false then {
    $sp := data→bd→create_picture($pic4);
    $sp→set_pos(620, $y);
    data→setCom1→add($sp);
  }
  // 2 clear
  $pic5→draw_text(28, 16, "Clear", 16, 0, colors→black);
  $sp := data→bd→create_picture($pic5);
  $sp→set_pos(400, $y);
  data→setCom1→add($sp);
  // 3 rand
  $pic6 := media→create_picture(150, 50);
  $pic6→fill_ellipse(0, 5, 90, 40, 0, colors→dark_gray);
  $pic6→draw_ellipse(2, 7, 86, 36, 0, $c, 1);
  $pic6→draw_ellipse(0, 0, 150, 50, 0, colors→light_gray, 1);
  $pic6→draw_text(28, 16, "Rand", 16, 0, colors→black);
  $pic6→fill_ellipse(88, 7, 60, 36, 0, colors→light_gray);
  $pic6→draw_text(109, 5, "+", 30, 0, colors→black);
  $sp := data→bd→create_picture($pic6);
  $sp→set_pos(565, $y);
  data→setCom1→add($sp);
  // 4 ok
  $pic3→fill_ellipse(0, 0, 90, 50, 0, colors→linear_gradient(colors→light_gray, colors→cyan, .5));
  $pic3→draw_ellipse(2, 2, 86, 46, 0, $c, 3);
  $pic3→draw_text(35, 17, "OK", 16, 0, colors→black);
  $sp := data→bd→create_picture($pic3);
  $sp→set_pos(730, $y);
  data→setCom1→add($sp);
  // Init
  meta private;
}
var traceSp0 : Number {
  // 2 ou 3 différents sprites
}
var bd : Board {
}
var pic : Picture {
}
var pic2 : Picture {
}
var p : Vector3 {
}
var Vie : Number_Map {
}
var Rang : Number_Map {
}
var Nait : Number_Map {
}
var pions0 : Sprite_Set {
}
var pions2 : Sprite_Set {
}
var pions4 : Sprite_Set {
}

action initData() {
  data→fr := 0;
  data→traceSp0 := 1;
  data→X := 500;
  data→Y := 500;
  data→d := 40;
  data→pas := 15;
  data→pas2 := 30;
  data→Nnes := 0;
  data→Nnew := 0;
  data→Ngen := 0;
  data→Tsp2 := 0;
  data→Tsp0 := 0;
  data→Tsp4 := 0;
  data→choix := 4;
  data→vu := 0;
  data→touch := 0;
  data→meme := 1;
  data→memeDiam := 0;
  data→initial := 1;
  data→replay := 0;
  data→showSp0 := 1;
  meta private;
}
var X : Number {
}
var Y : Number {
}
var pas : Number {
}
var Nnes : Number {
}
var Nnew : Number {
}
var Ngen : Number {
}
var Gene : Number_Map {
}
var Npar : Number_Map {
}

action trRecom(j: Number) {
  $dxy := data→Gene→at(data→generation * 1000 + $j);
  $d := math→floor($dxy / 1000000);
  $y := math→mod($dxy, 1000);
  $x := math→mod(($dxy - $y) / 1000, 1000);
  $x := 400 + ($x - data→X) * data→pas;
  $y := 240 + ($y - data→Y) * data→pas;
  if $d = 2 then {
    data→pions2→at(data→Tsp2)→set_pos($x, $y);
    data→pions2→at(data→Tsp2)→show;
    data→Tsp2 := data→Tsp2 + 1;
  }
  else {
    data→pions4→at(data→Tsp4)→set_pos($x, $y);
    data→pions4→at(data→Tsp4)→show;
    data→Tsp4 := data→Tsp4 + 1;
  }
  meta private;
}
var pas2 : Number {
}

action couleurs() {
  $t2 := 0;
  $t4 := 0;
  $t2, $t4 := code→bascule(0, $t2, $t4);
  // Couleurs initiales
  $c := colors→linear_gradient(colors→dark_gray, colors→brown, .75);
  $c1 := colors→linear_gradient(colors→light_gray, colors→brown, .5);
  $sp2 := data→SpCol→at(108);
  $sp4 := data→SpCol→at(107);
  $c2 := $sp4→color;
  if data→choix = 2 then {
    $c2 := $sp2→color;
  }
  // Sp sélection 2
  // Exemple
  for 0 ≤ i < 3 do {
    data→SpCol→at(109 + $i)→set_color(data→pions4→at(0)→color);
    data→SpCol→at(112 + $i)→set_color(data→pions2→at(0)→color);
  }
  // Sp100 centre color6
  $sp100 := data→SpCol→at(100);
  $sp100→set_color($c2);
  code→color6($c2, 4);
  data→bd→update_on_wall;
  $boucle := 0;
  while $boucle = 0 do {
    while data→bd→touched do {
      skip;
    }
    while data→bd→touched→equals(false) do {
      skip;
    }
    $p := data→bd→touch_start;
    data→spPos→set_pos($p→x, $p→y);
    if $p→x < 400 then {
      // choix couleur
      $boucleTest := 0;
      $test := 0;
      while $boucleTest = 0 and $test < 100 do {
        if data→spPos→overlaps_with(data→SpCol→at($test)) then {
          $boucleTest := 1;
          $c2 := data→SpCol→at($test)→color;
        }
        else {
          $test := $test + 1;
        }
      }
      if $test = 100 then {
        $c2 := colors→black;
      }
      $sp100→set_color($c2);
      code→color6($c2, data→choix);
      if data→choix = 2 then {
        $sp2→set_color($c2);
        for 0 ≤ i6 < 3 do {
          data→SpCol→at(112 + $i6)→set_color($c2);
        }
      }
      else {
        $sp4→set_color($c2);
        for 0 ≤ i7 < 3 do {
          data→SpCol→at(109 + $i7)→set_color($c2);
        }
      }
    }
    else {
      if $p→y > 420 then {
        // commandes
        if $p→x < 530 then {
          code→do_stuff2($c, $c1);
        }
        else {
          if $p→x < 650 then {
            for 0 ≤ i3 < 100 do {
              data→SpCol→at($i3)→set_color(colors→rand);
            }
          }
          else {
            $boucle := 1;
          }
        }
      }
      else {
        $select := - 1;
        for 0 ≤ j < 46 do {
          if data→spPos→overlaps_with(data→SpCol→at($j + 100)) then {
            $select := $j + 100;
          }
        }
        if $select = - 1 then {
          $c2 := colors→black;
          $sp100→set_color($c);
          code→color6($c2, data→choix);
        }
        else {
          if $select < 107 then {
            // fleur
            $c2 := data→SpCol→at($select)→color;
            code→color6($c2, data→choix);
            code→do_stuff2($c2, $c2);
          }
          else {
            if $select = 107 then {
              data→choix := 4;
              $sp4 := data→SpCol→at(107);
              $sp100→set_color($sp4→color);
              data→SpCol→at(145)→set_angle(0);
              data→SpCol→at(145)→set_pos(501, 370);
              code→color6($sp4→color, 4);
            }
            else {
              if $select = 108 then {
                data→choix := 2;
                $sp2 := data→SpCol→at(108);
                $sp100→set_color($sp2→color);
                data→SpCol→at(145)→set_angle(180);
                data→SpCol→at(145)→set_pos(579, 370);
                code→color6($sp2→color, 2);
              }
            }
          }
        }
      }
    }
    data→bd→update_on_wall;
  }
  for 0 ≤ i1 < 500 do {
    data→pions2→at($i1)→set_color($sp2→color);
    data→pions4→at($i1)→set_color($sp4→color);
  }
  code→bascule(1, $t2, $t4);
  //
  if false then {
    while false do {
      $p1 := data→bd→touch_start;
      if $p1→y > 580 and $p1→y < 640 then {
        if $p1→x < 160 then {
          if $p1→x > 80 then {
            skip;
          }
          data→bd→update_on_wall;
        }
        else {
          if $p1→x < 260 then {
            skip;
          }
          else {
            if $p1→x < 350 then {
              skip;
            }
          }
        }
      }
      else {
        if $p1→y < 400 and $p1→y > 15 and $p1→x > 35 and $p1→x < 425 then {
          $x1 := math→floor(($p1→x - 35) / 40) + math→floor(($p1→y - 15) / 40) * 10;
          $c2 := data→SpCol→at($x1)→color;
          $sp100→set_color($c2);
          code→color6($c2, data→choix);
        }
        else {
          if $p1→y > 640 then {
            $trans := (math→round($p1→x) / 800) + .2;
            $c2 := $sp100→color→make_transparent($trans);
            code→color6($c2, data→choix);
            if data→choix = 2 then {
              $sp2→set_color($c2);
              for 0 ≤ i5 < 3 do {
                data→SpCol→at(112 + $i5)→set_color($c2);
              }
            }
            else {
              $sp4→set_color($c2);
              for 0 ≤ i4 < 3 do {
                data→SpCol→at(109 + $i4)→set_color($c2);
              }
            }
          }
          else {
            $c2 := data→pic3→pixel($p1→x, $p1→y);
            if $c2→equals(colors→background) then {
              skip;
            }
            else {
              $sp100→set_color($c2);
              code→color6($c2, data→choix);
              if data→choix = 2 then {
                $sp2→set_color($c2);
                for 0 ≤ i2 < 3 do {
                  data→SpCol→at(112 + $i2)→set_color($c2);
                }
              }
              else {
                $sp4→set_color($c2);
                for 0 ≤ i10 < 3 do {
                  data→SpCol→at(109 + $i10)→set_color($c2);
                }
              }
            }
          }
        }
      }
      data→bd→update_on_wall;
    }
  }
  meta private;
}

action color6(c: Color, choix: Number) {
  data→SpCol→at(101)→set_color(colors→from_rgb($c→R - .15, $c→G, $c→B));
  data→SpCol→at(102)→set_color(colors→from_rgb($c→R + .15, $c→G, $c→B));
  data→SpCol→at(103)→set_color(colors→from_rgb($c→R, $c→G - .15, $c→B));
  data→SpCol→at(104)→set_color(colors→from_rgb($c→R, $c→G + .15, $c→B));
  data→SpCol→at(105)→set_color(colors→from_rgb($c→R, $c→G, $c→B - .15));
  data→SpCol→at(106)→set_color(colors→from_rgb($c→R, $c→G, $c→B + .15));
  code→transparence($c, $choix);
  meta private;
}
var pic3 : Picture {
}

action initCol() {
  data→SpCol := data→bd→create_sprite_set;
  data→pic3 := media→create_picture(data→bd→width, data→bd→height);
  data→pic3→clear(colors→black);
  $x := 0;
  $ang := 0;
  // 0 - 99
  for 0 ≤ i6 < 100 do {
    $sp1 := data→bd→create_ellipse(30, 30);
    data→SpCol→add($sp1);
    $sp1→set_color(colors→rand);
    $x := $i6 - math→floor($i6 / 10) * 10;
    $sp1→set_pos(35 + $x * 40, 60 + (($i6 - $x) * 4));
  }
  // 100 centre fleur
  $sp := data→bd→create_ellipse(30, 30);
  data→SpCol→add($sp);
  $sp→set_pos(600, 100);
  // 101 - 106 petale fleur
  for 0 ≤ i < 6 do {
    $ang := ($i * 60) * math→\u03C0 / 180;
    $sp2 := data→bd→create_ellipse(30, 30);
    data→SpCol→add($sp2);
    $sp2→set_pos(600 + 50 * math→cos($ang), 100 + 50 * math→sin($ang));
  }
  // 146
  $pic := media→create_picture(77, 54);
  $pic→fill_ellipse(0, 0, 154, 54, 0, colors→sepia);
  $pic→fill_ellipse(27, 9, 37, 37, 0, colors→black);
  $sprite := data→bd→create_picture($pic);
  $sprite→set_pos(501, 370);
  // 107 sp4
  $sp := data→bd→create_ellipse(30, 30);
  data→SpCol→add($sp);
  $sp→set_color(data→pions4→at(0)→color);
  $sp→set_pos(508, 370);
  // 108 sp2
  $sp := data→bd→create_ellipse(22, 22);
  data→SpCol→add($sp);
  $sp→set_color(data→pions2→at(0)→color);
  $sp→set_pos(572, 369);
  // 109 - 114 selection
  code→exTransparence(720, 350, 5);
  // 115 - 144
  for 0 ≤ i1 < 5 do {
    code→exTransparence(460 + $i1 * 72, 220, $i1);
  }
  // 145 cache selection sp2 sp4 initialise avant pour etre derriere
  data→SpCol→add($sprite);
  // sp4 sp2
  data→pic3→draw_ellipse(460, 340, 160, 60, 0, colors→accent, 3);
  data→pic3→draw_line(540, 340, 540, 400, colors→accent, 3);
  // commandes
  data→pic3→draw_ellipse(440, 430, 80, 40, 0, colors→accent, 3);
  data→pic3→draw_text(457, 440, "Initial", 20, 0, colors→sepia→lighten(0.2));
  data→pic3→draw_ellipse(560, 430, 80, 40, 0, colors→accent, 3);
  data→pic3→draw_text(577, 440, "Rand", 20, 0, colors→sepia→lighten(0.2));
  data→pic3→draw_ellipse(680, 430, 80, 40, 0, colors→accent, 3);
  data→pic3→draw_text(705, 440, "OK", 20, 0, colors→sepia→lighten(0.2));
  for 0 ≤ i2 < 146 do {
    data→SpCol→at($i2)→hide;
  }
  meta private;
}

action rand() {
  $x := 0;
  $y := 0;
  $x1 := 0;
  $y1 := 0;
  $px := 1;
  $py := 0;
  $xcote := math→floor(800 / data→pas2);
  $ycote := math→floor(480 / data→pas2);
  $Npos := math→floor($ycote / 10 * (7 + math→rand(20)));
  while $Npos > 3 do {
    $n := 2 + math→rand(7);
    $Npos := $Npos - $n;
    $x1 := math→rand($xcote);
    $y1 := math→rand($ycote);
    $px := - 1 * (math→floor($x1 / ($xcote / 3)) - 1);
    $py := - 1 * (math→floor($y1 / ($ycote / 3)) - 1);
    for 0 ≤ i < $n do {
      $x := math→floor((data→X + $x1 + $i * $px) - $xcote / 2);
      $y := math→floor((data→Y + $y1 + $i * $py) - $ycote / 2);
      if math→rand_norm > .8 then {
        if math→rand_norm > .5 then {
          $x := $x + 1;
        }
        else {
          $x := $x - 1;
        }
      }
      if math→rand_norm > .8 then {
        if math→rand_norm > .5 then {
          $y := $y + 1;
        }
        else {
          $y := $y - 1;
        }
      }
      data→Rang→set_at(data→Nnes, ($x * 1000) + $y);
      if data→Vie→at(data→Rang→at(data→Nnes)) = 0 then {
        data→Vie→set_at(data→Rang→at(data→Nnes), 1);
        data→Nnes := data→Nnes + 1;
      }
    }
  }
  meta private;
}

action clear() {
  for 0 ≤ i < data→Nnes do {
    data→Vie→set_at(data→Rang→at($i), 0);
    data→Rang→set_at($i, 0);
  }
  data→Nnes := 0;
  meta private;
}
var vu : Number {
}
var Vues : Number_Map {
}

action decal1() {
  $xDec := 0;
  $yDec := 0;
  if data→pions2→at(0)→is_visible then {
    $xDec := data→pions2→at(0)→x;
    $yDec := data→pions2→at(0)→y;
  }
  else {
    $xDec := data→pions4→at(0)→x;
    $yDec := data→pions4→at(0)→y;
  }
  for 0 ≤ i < data→Tsp2 do {
    data→pions2→at($i)→hide;
  }
  for 0 ≤ j < data→Tsp4 do {
    data→pions4→at($j)→hide;
  }
  code→decalRecom;
  if data→pions2→at(0)→is_visible then {
    $xDec := $xDec - data→pions2→at(0)→x;
    $yDec := $yDec - data→pions2→at(0)→y;
  }
  else {
    $xDec := $xDec - data→pions4→at(0)→x;
    $yDec := $yDec - data→pions4→at(0)→y;
  }
  data→X := data→X + math→floor($xDec / data→pas);
  data→Y := data→Y + math→floor($yDec / data→pas);
  data→bd→update_on_wall;
  meta private;
}

action diametre() {
  $d := data→d;
  $sprite := data→setCom2→at(3);
  if data→replay = 1 then {
    $sprite := data→setCom3→at(4);
  }
  while data→bd→touched do {
    if data→px > $sprite→x then {
      $d := $d + 1;
    }
    else {
      if $d > 14 then {
        $d := $d - 1;
      }
    }
    $d1 := $d;
    if data→memeDiam = 0 then {
      $d1 := math→floor($d * .75);
    }
    $d2 := math→floor($d * .5);
    for 0 ≤ i2 < 500 do {
      data→pions4→at($i2)→set_width($d);
      data→pions4→at($i2)→set_height($d);
      data→pions2→at($i2)→set_width($d1);
      data→pions2→at($i2)→set_height($d1);
      data→pions0→at($i2)→set_width($d2);
      data→pions0→at($i2)→set_height($d2);
    }
    data→bd→update_on_wall;
  }
  data→d := $d;
  meta private;
}

action sp0() {
  if data→px > data→setCom2→at(6)→x then {
    data→traceSp0 := 0;
    for 0 ≤ i5 < data→Tsp0 do {
      data→pions0→at($i5)→hide;
    }
    data→bd→update_on_wall;
  }
  else {
    if data→showSp0 = 1 then {
      for 0 ≤ i < data→Tsp0 do {
        data→pions0→at($i)→show;
      }
      data→bd→update_on_wall;
    }
    data→traceSp0 := 1;
  }
  meta private;
}

action step(n: Number, generation: Number) {
  if data→replay = 0 then {
    for 0 ≤ j < data→Tsp0 do {
      data→pions0→at($j)→hide;
    }
    data→bd→update_on_wall;
  }
  while data→bd→touched do {
    if $n = 1 then {
      data→pas := data→pas + 1;
    }
    else {
      if data→pas > 2 then {
        data→pas := data→pas - 1;
      }
    }
    $pion2 := 0;
    $pion4 := 0;
    $nTrace := data→Npar→at($generation);
    for 0 ≤ i < $nTrace do {
      $dxy := data→Gene→at($generation * 1000 + $i);
      $d := math→floor($dxy / 1000000);
      $y := math→mod($dxy, 1000);
      $x := math→mod(($dxy - $y) / 1000, 1000);
      $x := 400 + ($x - data→X) * data→pas;
      $y := 240 + ($y - data→Y) * data→pas;
      if $d = 2 then {
        data→pions2→at($pion2)→set_pos($x, $y);
        $pion2 := $pion2 + 1;
      }
      else {
        data→pions4→at($pion4)→set_pos($x, $y);
        $pion4 := $pion4 + 1;
      }
    }
    data→bd→update_on_wall;
    time→sleep(.06);
  }
  meta private;
}

action effa\u00E7age(t2: Number, t4: Number) returns t22: Number, t42: Number {
  if data→Tsp2 < $t2 then {
    for 0 ≤ i4 < $t2 - data→Tsp2 do {
      data→pions2→at(data→Tsp2 + $i4)→hide;
    }
  }
  $t22 := data→Tsp2;
  data→Tsp2 := 0;
  if data→Tsp4 < $t4 then {
    for 0 ≤ i1 < $t4 - data→Tsp4 do {
      data→pions4→at(data→Tsp4 + $i1)→hide;
    }
  }
  data→bd→update_on_wall;
  $t42 := data→Tsp4;
  data→Tsp4 := 0;
  meta private;
}

action initCom2() {
  data→setCom2 := data→bd→create_sprite_set;
  skip;
  // Nouvelle figure 0 0.0
  $y := 450;
  $yy := 402;
  $fond := data→piCom→clone;
  $fond→draw_text(39, 18, "New", 20, 0, colors→black);
  $sp := data→bd→create_picture($fond);
  $sp→set_pos(664, $yy);
  data→setCom2→add($sp);
  // Même figure 1 120.0
  $fond := data→piCom→clone;
  $fond→draw_text(22, 19, "  Same", 20, 0, colors→black);
  $sp := data→bd→create_picture($fond);
  $sp→set_pos(730, $y);
  data→setCom2→add($sp);
  // Mire centrage 2 240.0
  $fond := data→piCom→clone;
  $fond→draw_rect(50, 16, 20, 30, 0, colors→black, 3);
  $fond→draw_line(60, 4, 60, 56, colors→black, 1);
  $fond→draw_line(24, 31, 96, 31, colors→black, 1);
  $sp := data→bd→create_picture($fond);
  $sp→set_pos(730, $yy - ($y - $yy));
  data→setCom2→add($sp);
  // Diamètre 3 360.0
  $fond := data→piCom→clone;
  $fond→fill_rect(50, 0, 20, 60, 0, art→colCom);
  $fond→draw_text(19, 14, "-", 30, 0, colors→black);
  $fond→draw_text(84, 12, "+", 30, 0, colors→black);
  $fond→draw_text(49, 14, "O", 30, 0, colors→black);
  $fond→draw_line(50, 50, 70, 10, colors→black, 1);
  $sp := data→bd→create_picture($fond);
  $sp→set_pos(334, $y);
  data→setCom2→add($sp);
  // Espacement (step) 4 0.60
  $fond := data→piCom→clone;
  $fond→fill_rect(50, 0, 20, 60, 0, art→colCom);
  $fond→draw_text(73, 15, "<•>", 26, 0, colors→black);
  $fond→draw_text(8, 15, ">•<", 26, 0, colors→black);
  $sp := data→bd→create_picture($fond);
  $sp→set_pos(466, $y);
  data→setCom2→add($sp);
  // Recommencer 5 120.60
  $fond := data→piCom→clone;
  $fond→fill_rect(50, 0, 20, 60, 0, art→colCom);
  $fond→draw_text(33, 10, "Replay", 16, 0, colors→black);
  $fond→draw_text(12, 20, "<<   >>", 30, 0, colors→black);
  $sp := data→bd→create_picture($fond);
  $sp→set_pos(598, $y);
  data→setCom2→add($sp);
  // 2 ou 3 6 240.60
  $fond := data→piCom→clone;
  $fond→fill_rect(50, 0, 20, 60, 0, art→colCom);
  $fond→fill_ellipse(70, 18, 24, 24, 0, colors→linear_gradient(colors→light_gray, colors→brown, .7));
  $fond→fill_ellipse(90, 21, 18, 18, 0, colors→linear_gradient(colors→dark_gray, colors→brown, .75));
  $pic := data→piCom→clone;
  $fond→fill_ellipse(30, 18, 24, 24, 0, colors→linear_gradient(colors→light_gray, colors→brown, .7));
  $fond→fill_ellipse(10, 24, 12, 12, 0, colors→linear_gradient(colors→black, colors→light_gray, .5));
  $fond→fill_ellipse(20, 21, 18, 18, 0, colors→linear_gradient(colors→dark_gray, colors→brown, .75));
  $sp := data→bd→create_picture($fond);
  $sp→set_pos(70, $y);
  data→setCom2→add($sp);
  // Couleurs 7 360.60
  $fond := media→create_picture(120, 60);
  $pi := math→\u03C0 / 240;
  for 0 ≤ j < 120 do {
    $a := 30 * math→sin($j * 2 * $pi);
    $x := 60 + 60 * math→cos($j * 2 * $pi);
    $fond→draw_line($x, 30 + $a, $x, 30 - $a, colors→from_hsb(math→sin($j * $pi), 1, 1), 2);
  }
  $fond→draw_ellipse(2, 2, 116, 56, 0, colors→orange, 3);
  $sp := data→bd→create_picture($fond);
  $sp→set_pos(136, $yy);
  data→setCom2→add($sp);
  // Frein h 8 0.120
  $fond := data→piCom→clone;
  $fond→fill_rect(50, 0, 20, 60, 0, art→colCom);
  $fond→draw_text(35, 5, "Frein", 20, 0, colors→black);
  $fond→draw_text(19, 14, "-", 30, 0, colors→black);
  $fond→draw_text(82, 13, "+", 30, 0, colors→black);
  $sp := data→bd→create_picture($fond);
  $sp→set_pos(202, $y);
  data→setCom2→add($sp);
  // 9 texte frein
  $sp := data→bd→create_text(30, 30, 30, "0");
  $sp→set_color(colors→black);
  $sp→set_pos(202, $y);
  data→setCom2→add($sp);
  for 0 ≤ i < 10 do {
    data→setCom2→at($i)→set_opacity(0.6);
    data→setCom2→at($i)→hide;
  }
  meta private;
}
var SpCol : Sprite_Set {
}

action bascule(bascul: Number, t2: Number, t4: Number) returns t22: Number, t44: Number {
  $t22 := 0;
  $t44 := 0;
  if data→replay = 1 then {
    // Recom
    if $bascul = 0 then {
      for 0 ≤ k < 11 do {
        data→setCom3→at($k)→hide;
      }
      $t22, $t44 := code→efface;
      for 0 ≤ i11 < 146 do {
        data→SpCol→at($i11)→show;
      }
      data→bd→set_background_picture(data→pic3);
    }
    else {
      for 0 ≤ k1 < 11 do {
        data→setCom3→at($k1)→show;
      }
      for 0 ≤ i2 < $t2 do {
        data→pions2→at($i2)→show;
      }
      for 0 ≤ i3 < $t4 do {
        data→pions4→at($i3)→show;
      }
      for 0 ≤ i4 < 146 do {
        data→SpCol→at($i4)→hide;
      }
      data→bd→set_background_picture(data→pic);
    }
  }
  else {
    if $bascul = 0 then {
      for 0 ≤ k2 < 10 do {
        data→setCom2→at($k2)→hide;
      }
      for 0 ≤ i5 < data→Tsp2 do {
        data→pions2→at($i5)→hide;
      }
      for 0 ≤ i6 < data→Tsp4 do {
        data→pions4→at($i6)→hide;
      }
      for 0 ≤ j < data→Tsp0 do {
        data→pions0→at($j)→hide;
      }
      for 0 ≤ i7 < 146 do {
        data→SpCol→at($i7)→show;
      }
      data→bd→set_background_picture(data→pic3);
    }
    else {
      for 0 ≤ k3 < 10 do {
        data→setCom2→at($k3)→show;
      }
      for 0 ≤ j1 < data→Tsp0 do {
        data→pions0→at($j1)→show;
      }
      for 0 ≤ i8 < data→Tsp2 do {
        data→pions2→at($i8)→show;
      }
      for 0 ≤ i9 < data→Tsp4 do {
        data→pions4→at($i9)→show;
      }
      for 0 ≤ i10 < 146 do {
        data→SpCol→at($i10)→hide;
      }
      data→bd→set_background_picture(data→pic);
    }
  }
  data→bd→update_on_wall;
  meta private;
}

action frein(sprite: Sprite) {
  if data→px < $sprite→x then {
    if data→fr > 0 then {
      data→fr := data→fr - 1;
    }
  }
  else {
    data→fr := data→fr + 1;
  }
  data→setCom2→at(9)→set_text(data→fr ∥ "");
  data→setCom3→at(7)→set_text(data→fr ∥ "");
  data→bd→update_on_wall;
  time→sleep(.2);
  meta private;
}

action main() {
  wall→set_background(colors→gray);
  data→bd := invalid→board;
  code→init;
  code→initData;
  code→initCom1();
  code→initPions();
  code→initCom2();
  code→initCom3;
  code→initCol;
  code→initDepart;
  code→La_vie;
}
var v : Number {
}

action memeD() {
  $d := 0;
  if data→memeDiam = 0 then {
    if data→pions2→at(0)→width = data→pions4→at(0)→width then {
      $d := math→floor(data→pions4→at(0)→width * .75);
      for 0 ≤ i < 300 do {
        data→pions2→at($i)→set_width($d);
        data→pions2→at($i)→set_height($d);
      }
    }
  }
  else {
    if data→pions2→at(0)→width ≠ data→pions4→at(0)→width then {
      $d := data→pions4→at(0)→width;
      for 0 ≤ i1 < 300 do {
        data→pions2→at($i1)→set_width($d);
        data→pions2→at($i1)→set_height($d);
      }
    }
  }
  data→bd→update_on_wall;
  meta private;
}

action initPions() {
  $c := colors→linear_gradient(colors→dark_gray, colors→brown, .75);
  $c1 := colors→linear_gradient(colors→light_gray, colors→brown, .5);
  $c2 := colors→linear_gradient(colors→black, colors→dark_gray, .95)→make_transparent(0.2);
  for 0 ≤ i1 < 250 do {
    $sp := data→bd→create_ellipse(20, 20);
    $sp→set_color($c2);
    $sp→hide;
    data→pions0→add($sp);
    $sp := data→bd→create_ellipse(30, 30);
    $sp→set_color($c);
    $sp→hide;
    data→pions2→add($sp);
    $sp := data→bd→create_ellipse(40, 40);
    $sp→set_color($c1);
    $sp→hide;
    data→pions4→add($sp);
    $sp := data→bd→create_ellipse(40, 40);
    $sp→set_color($c1);
    $sp→hide;
    data→pions4→add($sp);
    $sp := data→bd→create_ellipse(30, 30);
    $sp→set_color($c);
    $sp→hide;
    data→pions2→add($sp);
    $sp := data→bd→create_ellipse(20, 20);
    $sp→set_color($c2);
    $sp→hide;
    data→pions0→add($sp);
  }
  meta private;
}

action depart(xy: Number) {
  data→Vie→set_at($xy, 1);
  data→Rang→set_at(data→Nnes, $xy);
  data→Nnes := data→Nnes + 1;
  meta private;
}

action initDepart() {
  code→depart(499499);
  code→depart(499500);
  code→depart(499501);
  code→depart(500501);
  code→depart(501501);
  code→depart(501500);
  code→depart(501499);
  meta private;
}

action exTransparence(x: Number, y: Number, index: Number) {
  // 80 482
  $index := math→round(10 * (.1 + ($index / 5))) / 10;
  for 0 ≤ i1 < 3 do {
    $sp := data→bd→create_ellipse(30, 30);
    $sp→set_color(data→pions4→at(0)→color→make_transparent($index));
    data→SpCol→add($sp);
  }
  $x1 := data→SpCol→count;
  data→SpCol→at($x1 - 3)→set_pos($x - 15, $y);
  data→SpCol→at($x1 - 2)→set_pos($x + 15, $y);
  data→SpCol→at($x1 - 1)→set_pos($x, $y + 15);
  // 112 113 114
  for 0 ≤ i3 < 3 do {
    $sp1 := data→bd→create_ellipse(22, 22);
    $sp1→set_color(data→pions2→at(0)→color→make_transparent($index));
    data→SpCol→add($sp1);
  }
  $x1 := data→SpCol→count;
  data→SpCol→at($x1 - 3)→set_pos($x, $y);
  data→SpCol→at($x1 - 2)→set_pos($x - 15, $y + 15);
  data→SpCol→at($x1 - 1)→set_pos($x + 15, $y + 15);
  meta private;
}

action transparence(c: Color, test: Number) {
  $rang := 115;
  for 0 ≤ i < 5 do {
    $index := math→round(10 * (.1 + ($i / 5))) / 10;
    if $test = 4 then {
      for 0 ≤ i1 < 3 do {
        $sp := data→SpCol→at($rang + $i1);
        $sp→set_color($c→make_transparent($index));
      }
      $rang := $rang + 6;
    }
    else {
      $rang := $rang + 3;
      for 0 ≤ i3 < 3 do {
        $sp2 := data→SpCol→at($rang + $i3);
        $sp2→set_color($c→make_transparent($index));
      }
      $rang := $rang + 3;
    }
  }
  meta private;
}
var choix : Number {
}
var piCom : Picture {
}
var v2 : Number {
}

action traceDecale() {
  $nSp0 := 0;
  $nSp2 := 0;
  $nSp4 := 0;
  $x := 0;
  $y := 0;
  for 0 ≤ i2 < data→Nnew do {
    $x := math→floor(data→Nait→at($i2) / 1000);
    $y := 400 - data→Y + (((data→Nait→at($i2) - $x * 1000))) * data→pas;
    $x := 240 - data→X + ($x) * data→pas;
    if data→Vie→at(data→Rang→at($i2)) < 3 then {
      data→pions2→at($nSp2)→set_pos($x, $y);
      data→pions2→at($nSp2)→show;
      $nSp2 := $nSp2 + 1;
    }
    else {
      data→pions4→at($nSp4)→set_pos($x, $y);
      data→pions4→at($nSp4)→show;
      $nSp4 := $nSp4 + 1;
    }
  }
  if data→traceSp0 = 1 then {
    for 0 ≤ i1 < data→Nnes do {
      if data→Vie→at(data→Rang→at($i1)) < 0 then {
        $x := math→floor(data→Rang→at($i1) / 1000);
        $y := (((data→Rang→at($i1) - $x * 1000)) - data→Y) * data→pas;
        $x := ($x - data→X) * data→pas;
        data→pions0→at($nSp0)→set_pos(240 + $x, 400 + $y);
        data→pions0→at($nSp0)→show;
        $nSp0 := $nSp0 + 1;
      }
    }
  }
  data→bd→update_on_wall;
  time→sleep(.005);
  meta private;
}

action decalRecom() {
  $xx := 0;
  $yy := 0;
  $y := 0;
  $x := 0;
  $dxy := 0;
  $d := 0;
  $pion2 := 0;
  $pion4 := 0;
  $nTrace := data→Npar→at(data→generation);
  for 0 ≤ i < $nTrace do {
    $dxy := data→Gene→at(data→generation * 1000 + $i);
    $d := math→floor($dxy / 1000000);
    if $d = 2 then {
      data→pions2→at($pion2)→show;
      $pion2 := $pion2 + 1;
    }
    else {
      data→pions4→at($pion4)→show;
      $pion4 := $pion4 + 1;
    }
  }
  $pion2 := 0;
  $pion4 := 0;
  while data→bd→touched do {
    for 0 ≤ i1 < $nTrace do {
      $xx := math→floor((data→bd→touch_current→x - 240)) / 10;
      $yy := math→floor((data→bd→touch_current→y - 400)) / 10;
      $dxy := data→Gene→at(data→generation * 1000 + $i1);
      $d := math→floor($dxy / 1000000);
      if $d = 2 then {
        $sprite := data→pions2→at($pion2);
        $sprite→set_pos($sprite→x - $xx, $sprite→y - $yy);
        $pion2 := $pion2 + 1;
      }
      else {
        $sprite1 := data→pions4→at($pion4);
        $sprite1→set_pos($sprite1→x - $xx, $sprite1→y - $yy);
        $pion4 := $pion4 + 1;
      }
    }
    $pion2 := 0;
    $pion4 := 0;
    data→bd→update_on_wall;
    time→sleep(.03);
  }
  meta private;
}
var setCom1 : Sprite_Set {
}
var setCom2 : Sprite_Set {
}

action reTrace(x: Number, y: Number) {
  for 0 ≤ i < data→Tsp2 do {
    data→pions2→at($i)→set_x(data→pions2→at($i)→x + $x);
    data→pions2→at($i)→set_y(data→pions2→at($i)→y + $y);
  }
  for 0 ≤ j < data→Tsp4 do {
    data→pions4→at($j)→set_x(data→pions4→at($j)→x + $x);
    data→pions4→at($j)→set_y(data→pions4→at($j)→y + $y);
  }
  if data→traceSp0 = 1 then {
    for 0 ≤ j1 < data→Tsp0 do {
      data→pions0→at($j1)→set_x(data→pions0→at($j1)→x + $x);
      data→pions0→at($j1)→set_y(data→pions0→at($j1)→y + $y);
    }
  }
  data→bd→update_on_wall;
  meta private;
}
var spText : Sprite {
}
var touch : Number {
}
var sens : Number {
}

action commandes() {
  // 0 new 1 meme 2 centrage 3 diametre 4 step 5 recomm 6 2 ou 3       7 couleur 8 et 9 frein
  // data boucle boucle boucle2 data tourne boucle select
  if data→touch = 1 then {
    // new
    data→tourne := 0;
    data→boucle := 1;
    data→meme := 2;
  }
  else {
    if data→touch = 2 then {
      // meme
      data→tourne := 0;
      data→boucle := 1;
      data→meme := 1;
    }
    else {
      if data→touch = 3 then {
        // centre
        code→reTrace((data→X - 500) * data→pas, (data→Y - 500) * data→pas);
        data→X := 500;
        data→Y := 500;
      }
      else {
        if data→touch = 4 then {
          // diametre
          code→diametre;
        }
        else {
          if data→touch = 5 then {
            // step
            if data→px > data→setCom2→at(4)→x then {
              // pas=pas+1
              code→step(1, data→Ngen - 1);
              data→showSp0 := 0;
            }
            else {
              // pas=pas-1
              code→step(0, data→Ngen - 1);
            }
          }
          else {
            if data→touch = 6 then {
              // recommencer
              data→tourne := 0;
              for 0 ≤ j < 10 do {
                data→setCom2→at($j)→hide;
              }
              if data→px > data→setCom2→at(5)→x then {
                data→sens := 1;
                data→replay := 1;
                code→boucleReplay;
                data→replay := 0;
              }
              else {
                data→replay := 1;
                data→sens := - 1;
                code→boucleReplay;
                data→replay := 0;
              }
            }
            else {
              if data→touch = 7 then {
                // 2 ou 3
                code→sp0;
              }
              else {
                if data→touch = 8 then {
                  // couleur
                  code→couleurs;
                }
                else {
                  // frein (8 et 9)
                  // sort de select sans eteindre sprites
                  data→tourne := 2;
                  code→frein(data→setCom2→at(8));
                }
              }
            }
          }
        }
      }
    }
  }
  if data→meme ≠ 0 then {
    for 0 ≤ i < 10 do {
      data→setCom2→at($i)→hide;
    }
    data→bd→update_on_wall;
  }
  while data→bd→touched do {
    skip;
  }
  meta private;
}
var px : Number {
}
var py : Number {
}

action boucle2() {
  $x := 0;
  $y := 0;
  data→meme := 0;
  $touch := 0;
  data→boucle := 0;
  while data→boucle = 0 do {
    data→bd→update_on_wall;
    code→vivent;
    if data→bd→touched then {
      code→select;
    }
    code→trace();
    skip;
    for 0 ≤ i2 < data→Nnew do {
      data→Vie→set_at(data→Nait→at($i2), 1);
      data→Rang→set_at($i2, data→Nait→at($i2));
    }
    data→Nnes := data→Nnew;
    data→bd→update_on_wall;
    if data→fr ≠ 0 then {
      code→pietine;
    }
  }
  for 0 ≤ i9 < data→Nnes do {
    data→Rang→set_at($i9, 0);
  }
  for 0 ≤ i3 < data→Nnew do {
    data→Vie→set_at(data→Nait→at($i3), 0);
    data→Nait→set_at($i3, 0);
    if $i3 < 500 then {
      data→pions0→at($i3)→hide;
      data→pions2→at($i3)→hide;
      data→pions4→at($i3)→hide;
    }
  }
  data→bd→update_on_wall;
  while data→bd→touched do {
    skip;
  }
  meta private;
}

action init() {
  wall→clear;
  data→bd := media→create_landscape_board(800, 480);
  data→pic := media→create_picture(data→bd→width, data→bd→height);
  data→bd→set_background_picture(data→pic);
  data→pic2 := media→create_picture(data→bd→width, data→bd→height);
  data→Vie := math→create_number_map;
  data→Rang := math→create_number_map;
  data→Nait := math→create_number_map;
  data→Gene := math→create_number_map;
  data→Npar := math→create_number_map;
  data→Vues := math→create_number_map;
  data→Mere := collections→create_number_map;
  data→pions0 := data→bd→create_sprite_set;
  data→pions2 := data→bd→create_sprite_set;
  data→pions4 := data→bd→create_sprite_set;
  data→p := math→create_vector3(0, 0, 0);
  data→spText := data→bd→create_text(100, 20, 40, "");
  data→spText→set_color(colors→orange);
  data→spText→set_y(100);
  data→spPos := data→bd→create_rectangle(2, 2);
  data→spPos→set_pos( - 2, - 2);
  data→spPos→set_opacity(0);
  data→piCom := media→create_picture(120, 60);
  data→piCom→fill_ellipse(0, 0, 120, 60, 0, art→colCom);
  data→piCom→draw_ellipse(2, 2, 116, 56, 0, colors→orange, 3);
  data→pixel := media→create_picture(3, 3);
  data→pixel→clear(colors→orange);
  meta private;
}
var d : Number {
}
var meme : Number {
}

event gameloop() {
  $test := 0;
  if data→meme = 1 and data→initial = 0 then {
    code→Figure();
    time→sleep(.3);
  }
  for 0 ≤ i < data→Nnes do {
    data→Mere→set_at($i, data→Rang→at($i));
    // (i+data Ngen*1000) avec data Ngen=0
    data→Gene→set_at($i, 4000000 + data→Rang→at($i));
  }
  $MereNes := data→Nnes;
  data→Npar→set_at(0, data→Nnes);
  data→Ngen := 1;
  $Xne := data→X;
  $Yne := data→Y;
  if data→initial = 0 then {
    code→boucle2;
  }
  else {
    data→initial := 0;
  }
  if data→meme = 1 then {
    time→sleep(.3);
    code→quad();
    data→Nnes := $MereNes;
    for 0 ≤ i2 < data→Nnes do {
      data→Rang→set_at($i2, data→Mere→at($i2));
      data→Vie→set_at(data→Rang→at($i2), 1);
    }
    for 0 ≤ i3 < 5 do {
      data→setCom1→at($i3)→show;
    }
    data→X := $Xne;
    data→Y := $Yne;
    code→tracell;
  }
  else {
    data→X := 500;
    data→Y := 500;
    data→Nnes := 0;
    code→rand;
    for 0 ≤ i1 < data→Nnes do {
      data→Vie→set_at(data→Rang→at($i1), 1);
    }
    data→Ngen := 0;
  }
  data→Nnew := 0;
  data→bd→update_on_wall;
  meta private;
}
var Mere : Number_Map {
}
var initial : Number {
}
var spPos : Sprite {
}

action select() {
  data→tourne := 1;
  while data→tourne = 1 do {
    if data→setCom2→at(0)→is_visible then {
      data→px := data→bd→touch_start→x;
      data→py := data→bd→touch_start→y;
      data→spPos→set_pos(data→px, data→py);
      data→touch := 0;
      for 0 ≤ j < 10 do {
        if data→spPos→overlaps_with(data→setCom2→at($j)) then {
          data→touch := $j + 1;
        }
      }
      if data→touch ≠ 0 then {
        code→commandes;
        if data→meme ≠ 0 then {
          data→tourne := 0;
        }
        else {
          if data→tourne = 1 then {
            while data→bd→touched do {
              skip;
            }
            while data→bd→touched→equals(false) do {
              skip;
            }
          }
        }
      }
      else {
        time→sleep(.3);
        if data→bd→touched then {
          code→decal();
          while data→bd→touched do {
            skip;
          }
          while data→bd→touched→equals(false) do {
            skip;
          }
        }
        else {
          data→tourne := 0;
        }
      }
    }
    else {
      time→sleep(.3);
      if data→bd→touched then {
        code→decal();
        data→tourne := 0;
      }
      else {
        for 0 ≤ i < 10 do {
          data→setCom2→at($i)→show;
        }
        data→bd→update_on_wall;
        while data→bd→touched do {
          skip;
        }
        while data→bd→touched→equals(false) do {
          skip;
        }
      }
    }
  }
  if data→traceSp0 = 1 then {
    for 0 ≤ j1 < data→Tsp0 do {
      data→pions0→at($j1)→show;
    }
  }
  data→showSp0 := 1;
  if data→tourne ≠ 2 then {
    for 0 ≤ i1 < 10 do {
      data→setCom2→at($i1)→hide;
    }
  }
  data→bd→update_on_wall;
  while data→bd→touched do {
    skip;
  }
  //

  meta private;
}
var boucle : Number {
}
var tourne : Number {
}
var replay : Number {
}

action comReplay(t2: Number, t4: Number) {
  $retrace := 0;
  if data→touch = 0 then {
    // lecture (com2)
    $retrace := code→pas_a_pas;
  }
  else {
    if data→touch = 1 then {
      // frein
      code→frein(data→setCom3→at(1));
      data→tourne := 2;
    }
    else {
      if data→touch = 2 then {
        // stop
        data→memeDiam := 0;
        code→memeD;
        data→tourne := 0;
        data→tourneReplay := 0;
      }
      else {
        if data→touch = 3 then {
          // step
          if data→px > data→setCom3→at(3)→x then {
            code→step(1, data→generation);
          }
          else {
            code→step(0, data→generation);
          }
        }
        else {
          if data→touch = 4 then {
            // diametre
            code→diametre;
          }
          else {
            if data→touch = 5 then {
              // couleur
              code→couleurs;
            }
            else {
              if data→touch = 6 then {
                // centrage
                data→X := 500;
                data→Y := 500;
                $retrace := 1;
              }
              else {
                if data→touch = 7 then {
                  // frein
                  code→frein(data→setCom3→at(1));
                  data→tourne := 2;
                }
                else {
                  if data→touch = 8 then {
                    // pion2=pion4
                    if data→px > data→setCom3→at(8)→x then {
                      data→memeDiam := 0;
                    }
                    else {
                      data→memeDiam := 1;
                    }
                    code→memeD;
                    // selon meme diametre en replay
                  }
                  else {
                    if data→touch = 9 then {
                      // lecture avant
                      skip;
                      if data→px > data→setCom3→at(9)→x then {
                        data→sens := 1;
                        data→tourne := 0;
                      }
                      else {
                        code→efface;
                        data→generation := 0;
                        $retrace := 1;
                      }
                    }
                    else {
                      // lecture arriere
                      if data→px < data→setCom3→at(10)→x then {
                        data→sens := - 1;
                        data→tourne := 0;
                      }
                      else {
                        code→efface;
                        data→generation := data→Ngen - 1;
                        $retrace := 1;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  if $retrace ≠ 0 then {
    data→Tsp2 := 0;
    data→Tsp4 := 0;
    for 0 ≤ i < data→Npar→at(data→generation) do {
      code→trRecom($i);
    }
    data→bd→update_on_wall;
  }
  meta private;
}

action boucleReplay() {
  $t2 := 0;
  $nsp2 := data→Tsp2;
  for 0 ≤ i5 < $nsp2 do {
    data→pions2→at($i5)→hide;
  }
  data→Tsp2 := 0;
  $t4 := 0;
  $nsp4 := data→Tsp4;
  for 0 ≤ i6 < $nsp4 do {
    data→pions4→at($i6)→hide;
  }
  data→Tsp4 := 0;
  for 0 ≤ i1 < data→Tsp0 do {
    data→pions0→at($i1)→hide;
  }
  data→bd→update_on_wall;
  //
  $aff := 0;
  $frein := 0;
  data→generation := 0;
  $Nsp := 0;
  $p := data→bd→touch_start;
  $p1 := data→bd→touch_end;
  data→sens0 := data→sens;
  //
  $limit := data→Ngen - 1;
  if data→sens = - 1 then {
    data→generation := $limit;
  }
  else {
    data→generation := 0;
  }
  data→tourneReplay := 1;
  while data→bd→touched do {
    skip;
  }
  while data→generation ≥ 0 and data→generation ≤ $limit and data→tourneReplay = 1 do {
    //
    for 0 ≤ i < data→Npar→at(data→generation) do {
      code→trRecom($i);
    }
    $t2, $t4 := code→effa\u00E7age($t2, $t4);
    //
    if data→fr ≠ 0 then {
      code→pietine;
    }
    if data→bd→touched then {
      code→selectReplay($t2, $t4);
    }
    data→generation := data→generation + data→sens;
  }
  if $t2 > $nsp2 then {
    for 0 ≤ i2 < $t2 - $nsp2 do {
      data→pions2→at($nsp2 + $i2)→hide;
    }
  }
  data→Tsp2 := $nsp2;
  if $t4 > $nsp4 then {
    for 0 ≤ i3 < $t4 - $nsp4 do {
      data→pions4→at($nsp4 + $i3)→hide;
    }
  }
  data→Tsp4 := $nsp4;
  if data→sens = - 1 and data→tourneReplay = 1 then {
    data→sens := 1;
    code→boucleReplay;
  }
  for 0 ≤ j < 11 do {
    data→setCom3→at($j)→hide;
  }
  data→bd→update_on_wall;
  meta private;
}

action selectReplay(t2: Number, t4: Number) {
  data→tourne := 1;
  while data→tourne = 1 do {
    if data→setCom3→at(0)→is_visible then {
      data→px := data→bd→touch_start→x;
      data→py := data→bd→touch_start→y;
      data→spPos→set_pos(data→px, data→py);
      data→touch := - 1;
      for 0 ≤ j < 11 do {
        if data→spPos→overlaps_with(data→setCom3→at($j)) then {
          data→touch := $j;
        }
      }
      if data→touch ≠ - 1 then {
        code→comReplay($t2, $t4);
        if data→tourne = 1 then {
          while data→bd→touched do {
            skip;
          }
          while data→bd→touched→equals(false) do {
            skip;
          }
        }
      }
      else {
        time→sleep(.3);
        if data→bd→touched then {
          code→decal1;
          while data→bd→touched do {
            skip;
          }
          while data→bd→touched→equals(false) do {
            skip;
          }
        }
        else {
          for 0 ≤ k < 11 do {
            data→setCom3→at($k)→hide;
          }
          data→tourne := 0;
          data→sens := data→sens0;
        }
      }
    }
    else {
      time→sleep(.3);
      if data→bd→touched then {
        code→decal1;
        data→tourne := 0;
      }
      else {
        for 0 ≤ i < 11 do {
          data→setCom3→at($i)→show;
        }
        data→bd→update_on_wall;
        while data→bd→touched do {
          skip;
        }
        while data→bd→touched→equals(false) do {
          skip;
        }
      }
    }
  }
  if data→tourne ≠ 2 then {
    for 0 ≤ i1 < 10 do {
      data→setCom2→at($i1)→hide;
    }
  }
  data→bd→update_on_wall;
  while data→bd→touched do {
    skip;
  }
  meta private;
}
var tourneReplay : Number {
}

action efface() returns t2: Number, t4: Number {
  $t2 := 0;
  $t4 := 0;
  for 0 ≤ k < data→Npar→at(data→generation) do {
    $dxy := data→Gene→at(data→generation * 1000 + $k);
    $d := math→floor($dxy / 1000000);
    if $d = 2 then {
      data→pions2→at($t2)→hide;
      $t2 := $t2 + 1;
    }
    else {
      data→pions4→at($t4)→hide;
      $t4 := $t4 + 1;
    }
  }
  meta private;
}
var generation : Number {
}

action pas_a_pas() returns retrace: Number {
  $retrace := 0;
  $play := 0;
  $spX := data→setCom3→at(0)→x;
  if math→abs(data→px - $spX) > 25 then {
    if data→px > $spX then {
      if data→px > $spX + 75 then {
        data→sens := 1;
        $play := 1;
      }
      else {
        code→efface;
        if data→generation < data→Ngen - 1 then {
          data→generation := data→generation + 1;
        }
        data→sens := 0;
        $retrace := 1;
      }
    }
    else {
      if data→px < $spX - 75 then {
        data→sens := - 1;
        $play := 1;
      }
      else {
        code→efface;
        if data→generation > 0 then {
          data→generation := data→generation - 1;
        }
        data→sens := 0;
        $retrace := 1;
      }
    }
  }
  else {
    data→sens := 0;
  }
  if $play = 1 then {
    code→efface;
    while data→bd→touched and $play = 1 do {
      code→efface;
      data→generation := data→generation + data→sens;
      if data→generation ≥ 0 and data→generation ≤ data→Ngen - 1 then {
        data→Tsp2 := 0;
        data→Tsp4 := 0;
        for 0 ≤ i < data→Npar→at(data→generation) do {
          code→trRecom($i);
        }
        data→bd→update_on_wall;
        if data→fr ≠ 0 then {
          $wait := data→fr * 20000;
          while $wait > 0 and data→bd→touched do {
            $wait := $wait - 1;
          }
        }
      }
      else {
        $play := 0;
        data→generation := data→generation - data→sens;
        data→sens := 0;
      }
    }
  }
  meta private;
}
var memeDiam : Number {
}

action pietine() {
  $wait := data→fr * 20000;
  while $wait > 0 and data→bd→touched→equals(false) do {
    $wait := $wait - 1;
  }
  meta private;
}

action initCom3() {
  // Lecture recom 10 0.180
  data→setCom3 := data→bd→create_sprite_set;
  $pic2 := media→create_picture(300, 60);
  $pic2→fill_ellipse(0, 0, 300, 60, 0, colors→light_gray);
  for 0 ≤ j < 3 do {
    $pic2→draw_ellipse($j + 1, $j, 298 - $j * 2, 60 - $j * 2, 0, colors→orange, 1);
  }
  $pic2→fill_ellipse(75, 0, 150, 60, 0, colors→dark_gray);
  $pic2→draw_ellipse(78, 3, 144, 54, 0, colors→brown, 3);
  $pic2→fill_rect(140, 0, 20, 60, 0, colors→dark_gray);
  $pic2→draw_text(5, 8, "<<", 40, 0, colors→black);
  $pic2→draw_text(87, 8, "<", 40, 0, colors→black);
  $pic2→draw_text(185, 8, ">", 40, 0, colors→black);
  $pic2→draw_text(240, 8, ">>", 40, 0, colors→black);
  $pic2→draw_ellipse(125, 5, 50, 50, 0, colors→black, 2);
  $pic2→draw_text(130, 18, "Stop", 20, 0, colors→black);
  $sp := data→bd→create_picture($pic2);
  $sp→set_y(700);
  data→setCom3→add($sp);
  // Frein v 11 60.300
  $pic2 := data→setCom2→at(8)→picture→clone;
  $sp := data→bd→create_picture($pic2);
  $sp→set_pos(200, 450);
  data→setCom3→add($sp);
  // Stop 12 360.300
  $pic2 := media→create_picture(120, 60);
  $pic2→fill_ellipse(0, 0, 120, 60, 0, colors→linear_gradient(colors→dark_gray, colors→red, .7));
  $pic2→draw_ellipse(3, 3, 114, 54, 0, colors→orange, 3);
  $pic2→draw_text(25, 17, "BACK", 27, 0, colors→black);
  $sp := data→bd→create_picture($pic2);
  $sp→set_pos(730, 310);
  data→setCom3→add($sp);
  // 13 step 120.300
  $pic2 := data→setCom2→at(4)→picture→clone;
  $sp := data→bd→create_picture($pic2);
  $sp→set_pos(600, 450);
  data→setCom3→add($sp);
  // Diamètre 14 180.300
  $pic2 := data→setCom2→at(3)→picture→clone;
  $sp := data→bd→create_picture($pic2);
  $sp→set_pos(470, 450);
  data→setCom3→add($sp);
  // Couleurs 15 240.300
  $pic2 := data→setCom2→at(7)→picture→clone;
  $sp := data→bd→create_picture($pic2);
  $sp→set_pos(70, 380);
  data→setCom3→add($sp);
  // Centrage 16 300.300
  $pic2 := data→setCom2→at(2)→picture→clone;
  $sp := data→bd→create_picture($pic2);
  $sp→set_y(405);
  data→setCom3→add($sp);
  // Frein 17 40.680
  $sp := data→bd→create_text(30, 30, 30, "");
  $sp→set_color(colors→black);
  $sp→set_pos(200, 450);
  data→setCom3→add($sp);
  // 18 2 même diamètre 420 300
  $pic2 := data→piCom→clone;
  $pic2→fill_ellipse(70, 18, 24, 24, 0, colors→linear_gradient(colors→light_gray, colors→brown, .7));
  $pic2→fill_ellipse(90, 21, 18, 18, 0, colors→linear_gradient(colors→dark_gray, colors→brown, .75));
  $pic2→fill_ellipse(26, 18, 24, 24, 0, colors→linear_gradient(colors→light_gray, colors→brown, .7));
  $pic2→fill_ellipse(10, 18, 24, 24, 0, colors→linear_gradient(colors→dark_gray, colors→brown, .75));
  $sp := data→bd→create_picture($pic2);
  $sp→set_pos(70, 450);
  data→setCom3→add($sp);
  // 19 lecture avant 0.500
  $pic2 := media→create_picture(150, 60);
  $pic2→fill_ellipse(0, 0, 150, 60, 0, colors→dark_gray);
  $pic2→draw_ellipse(3, 3, 144, 54, 0, colors→orange, 3);
  $pic2→fill_rect(65, 0, 20, 60, 0, colors→dark_gray);
  $pic2→draw_text(105, 7, ">", 42, 0, colors→black);
  $pic2→draw_line(105, 19, 105, 41, colors→black, 4);
  $pic2→draw_rect(25, 15, 10, 30, 0, colors→black, 1);
  $pic2→draw_line(35, 29, 60, 29, colors→black, 2);
  $sp := data→bd→create_picture($pic2);
  $sp→set_pos(85, 770);
  data→setCom3→add($sp);
  // Lecture arrière 20 60.500
  $pic2 := media→create_picture(150, 60);
  $pic2→blend(data→setCom3→at(9)→picture, 150, 60, 180, 1);
  $sp := data→bd→create_picture($pic2);
  $sp→set_pos(395, 770);
  data→setCom3→add($sp);
  for 0 ≤ i < 11 do {
    data→setCom3→at($i)→set_opacity(0.5);
    data→setCom3→at($i)→hide;
  }
  meta private;
}
var setCom3 : Sprite_Set {
}
var colCom : Color {
  is\_resource = true;
  url = "#ffb1e1e7";
}
var showSp0 : Number {
}
var pixel : Picture {
}
var sens0 : Number {
}

action do_stuff(p: Number) {
  data→spText→set_text($p→to_string);
  data→bd→update_on_wall;
  meta private;
}

action do_stuff2(c1: Color, c2: Color) {
  if data→choix = 4 then {
    data→SpCol→at(107)→set_color($c1);
    data→SpCol→at(100)→set_color($c1);
    code→color6($c1, 4);
    for 0 ≤ i8 < 3 do {
      data→SpCol→at(109 + $i8)→set_color($c1);
    }
  }
  else {
    data→SpCol→at(108)→set_color($c2);
    data→SpCol→at(100)→set_color($c2);
    code→color6($c2, 2);
    for 0 ≤ i9 < 3 do {
      data→SpCol→at(112 + $i9)→set_color($c2);
    }
  }
  meta private;
}

