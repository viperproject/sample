meta version "v2.2,nothing";
meta name "turning 3d";
meta icon "clover";
meta color "#ffdaa520";
// there is a problem with rotations , could be inverted
meta platform "current";

action main() {
  // slideshow
  "TouchDevelop is cool" →post_to_wall;
  code→initData;
  code→init;
  data→rang := records→t_index→at(0)→total→get;
  if data→rang > 0 then {
    data→rang := data→rang - 1;
    data→same := 0;
    data→theta := 0;
    data→phi := 0;
    code→drawSame;
    code→restart;
  }
  else {
    code→initPicShow;
    data→lecture := 0;
    code→posSp;
  }
}
var theta : Number {
}
var phi : Number {
}
table t {
  type = "Index";
  keys {
    n: Number
  }
  fields {
    X: Number_field
    Y: Number_field
    Z: Number_field
    c: Color_field
    ecart: Number_field
    pic: Picture_field
    p\'titePic: Picture_field
    total: Number_field
    prog: Number_field
    rangSp0: Number_field
    N: Number_field
    forme: Number_field
    structure: Number_field
  }
}

action init() {
  wall→set_background(colors→black);
  data→bd := media→create_portrait_board(480, 800);
  data→pic := media→create_picture(480, 800);
  data→pic→clear(colors→black);
  data→bd→set_background_picture(data→pic);
  data→bd→post_to_wall;
  data→p\'titePic := media→create_picture(120, 200);
  $pic := media→create_picture(70, 4);
  data→sepia := colors→sepia→lighten(0.2);
  $pic→fill_ellipse(0, 0, 4, 4, 0, data→sepia);
  $pic→fill_ellipse(66, 0, 4, 4, 0, data→sepia);
  $pic→fill_rect(2, 0, 66, 4, 0, data→sepia);
  data→setSpProg := data→bd→create_sprite_set;
  for 0 ≤ i < 6 do {
    $sp := data→bd→create_picture($pic→clone);
    $sp→set_pos(37 + $i * 72, 300);
    $sp→set_opacity(0.4);
    $sp→hide;
    data→setSpProg→add($sp);
  }
  data→setSpProg→at(2)→set_opacity(1);
  data→c := colors→from_hsb(math→random_normalized, .9, .8);
  data→setLine := data→bd→create_sprite_set;
  for 0 ≤ i2 < 4 do {
    $sp2 := data→bd→create_rectangle(110, 1 + $i2);
    $sp2→set_pos(70, 95 + $i2 * 45);
    $sp2→set_color(data→c);
    $sp2→set_opacity(.3);
    data→setLine→add($sp2);
    data→setSpProg→add($sp2);
  }
  data→setLine→at(0)→set_opacity(1);
  data→d := 1;
  data→spSet := data→bd→create_sprite_set;
  for 0 ≤ i1 < 20 do {
    $sp1 := data→bd→create_ellipse(24, 24);
    $sp1→hide;
    data→spSet→add($sp1);
  }
  data→setAtome := data→bd→create_sprite_set;
  for 0 ≤ i3 < 20 do {
    $dia := .5 + math→floor($i3 / 4) * .125;
    $dia := 8 + 16 * $dia;
    $sp3 := data→bd→create_ellipse($dia, $dia);
    $sp3→hide;
    data→setAtome→add($sp3);
  }
  code→spaceSp;
  code→visibles;
  data→ecart := .2;
  code→initFond;
  code→initForme;
  code→initCurseur;
  code→initSpCom;
  code→initMap;
  code→initTint\u0026speed;
  data→setSpProg→add(data→spStart);
  code→setColSp;
  code→initSetRot;
  code→showSp(0);
  code→initSpProg;
  meta private;
}
var bd : Board {
}
var spSet : Sprite_Set {
}
var pic : Picture {
}

action posSp() {
  $rang := records→t_index→at(0)→total→get;
  code→choixCouleur($rang);
  // Autre que "back"
  if data→new = 1 then {
    $rangSp0 := records→t_index→at($rang)→rangSp0→get;
    if data→same = 0 then {
      if data→structure = 0 then {
        code→posRandom($rangSp0);
      }
      else {
        if data→structure = 1 then {
          code→posAtome($rangSp0);
        }
        else {
          code→posRuban($rangSp0);
        }
      }
    }
    else {
      data→structure := records→t_index→at(data→rang)→structure→get;
      code→copyXYZ($rangSp0);
      code→rotations($rangSp0);
    }
    data→rang := $rang;
    data→rangSp0 := $rangSp0;
    code→setMaps($rangSp0);
    while data→bd→touched do {
      skip;
    }
    if data→same = 0 or data→changeProg = 1 then {
      code→setProg;
      data→changeProg := 0;
    }
    code→colSpCom;
    code→affichage(1);
    data→theta := 0;
    data→phi := 0;
    data→same := 0;
    data→stop := 0;
  }
  meta private;
}

action doRotate(i: Number) returns x: Number, y: Number {
  // By Terry123
  $y := 300 + (math→sin(data→theta) * data→mx→at($i) + math→cos(data→theta) * data→my→at($i));
  $x2 := math→sin(data→theta) * data→my→at($i) - math→cos(data→theta) * data→mx→at($i);
  $x := 240 + (math→cos(data→phi) * data→mz→at($i) + math→sin(data→phi) * $x2);
  meta private;
}
var N : Number {
}

action restart() {
  data→theta := 0;
  data→phi := 0;
  $total := records→t_index→at(0)→total→get;
  if data→stop ≠ - 1 then {
    data→bd→set_background_picture(data→picShow);
    data→bd→update_on_wall;
    while data→bd→touched do {
      skip;
    }
    while data→bd→touched→equals(false) do {
      skip;
    }
    $p := data→bd→touch_start;
    $select := (math→floor($p→x / 120)) + 4 * (math→floor($p→y / 200));
    $press := code→bdPressed;
    if code→bdPressed = 1 and $select < $total then {
      code→erase($select);
      // Gameloop avec data new=0
    }
    else {
      if $select > $total - 1 or $select > 14 then {
        code→newStart;
      }
      else {
        data→rang := $select;
        code→reposSp;
        if $total > 1 then {
          data→lecture := 1;
          code→colSpCom;
          data→spCom→show;
        }
      }
    }
  }
  else {
    code→newStart;
  }
  meta private;
}

action zzzzz() {
  skip;
  meta private;
}
var prog : Number {
}
var progH : Number {
}
var progV : Number {
}
var new : Number {
}

action reposSp() {
  $rang := records→t_index→at(data→rang)→rangSp0→get;
  data→prog := records→t_index→at(data→rang)→prog→get;
  data→c := records→t_index→at(data→rang)→c→get;
  data→ecart := records→t_index→at(data→rang)→ecart→get;
  code→changeCommun(1, 0);
  code→changeCurseurs;
  data→progH := math→\u03C0 / data→prog;
  data→progV := data→progH * data→V;
  data→progH := data→progH * data→H;
  data→pic := records→t_index→at(data→rang)→pic→get→clone;
  data→bd→set_background_picture(data→pic);
  for 0 ≤ i < data→N do {
    data→spSet→at($i)→hide;
  }
  data→bd→update_on_wall;
  code→setColSp;
  code→setMaps($rang);
  while data→bd→touched do {
    skip;
  }
  for 0 ≤ i1 < 9 do {
    data→setSpeed→at($i1)→set_opacity(.5);
  }
  data→setSpeed→at(0)→set_opacity(1);
  data→speedA := 0;
  for 0 ≤ i2 < data→N do {
    data→spSet→at($i2)→show;
  }
  code→replay;
  meta private;
}
var picShow : Picture {
}

action record() returns total: Number {
  $n := records→t_index→at(0)→total→get;
  records→t_index→at($n)→c→set(data→c);
  records→t_index→at($n)→ecart→set(data→ecart);
  records→t_index→at($n)→N→set(data→N);
  $forme := (data→forme * 100) + (data→nProg * 10);
  if data→structure < 2 then {
    $forme := $forme + data→irise;
  }
  else {
    $forme := $forme + data→tour;
  }
  records→t_index→at($n)→forme→set($forme);
  records→t_index→at($n)→structure→set(data→structure);
  records→t_index→at($n)→prog→set(data→prog);
  $pic := data→pic→clone;
  records→t_index→at($n)→pic→set($pic);
  $rang := records→t_index→at($n)→rangSp0→get;
  records→t_index→at($n + 1)→rangSp0→set($rang + data→N);
  $y := math→floor($n / 4);
  $x := $n - $y * 4;
  $pic := data→pic→clone;
  $pic→resize(120, 200);
  records→t_index→at($n)→p\'titePic→set($pic);
  if $n < 15 then {
    data→picShow→blend($pic, $x * 120, $y * 200, 0, 1);
    data→picShow→update_on_wall;
  }
  data→rang := $n;
  $n := $n + 1;
  records→t_index→at(0)→total→set($n);
  $total := $n;
  data→back := 1;
  meta private;
}

action choixCouleur(total: Number) {
  code→startChoix;
  while data→new = 0 do {
    while data→bd→touched→equals(false) do {
      skip;
    }
    $p := data→bd→touch_start;
    if $p→y < 280 and $p→x < 430 then {
      if $p→x < 160 then {
        if $p→y > 70 then {
          code→setD;
        }
        else {
          if $total > 0 then {
            if data→back = 1 then {
              if data→rang > 0 then {
                data→rang := data→rang - 1;
              }
              data→back := 0;
            }
            data→new := 2;
          }
        }
      }
      else {
        if $p→x < 365 then {
          // Set data new=1
          code→voisine2;
        }
        else {
          code→setForme(math→floor((data→bd→touch_start→y - 15) / 45));
          data→bd→update_on_wall;
        }
      }
    }
    else {
      while data→bd→touched do {
        if $p→x > 440 then {
          code→setColor;
        }
        else {
          if $p→y > 630 then {
            if $p→x > 240 then {
              // Set data new=1
              code→setStart($p→y);
            }
            else {
              // Random Atome Ruban
              code→setStructure($p→y);
            }
          }
          else {
            if $p→y > 570 then {
              code→setEcart;
            }
            else {
              if $p→y > 480 then {
                if data→structure < 2 then {
                  code→setIrise($p→x);
                }
                else {
                  code→setComplex($p→x);
                }
              }
              else {
                if $p→y > 430 then {
                  code→setN;
                }
                else {
                  if $p→y > 340 then {
                    if data→irise = 0 then {
                      code→coloRand;
                    }
                  }
                  else {
                    if $p→y > 270 then {
                      code→set_nProg;
                    }
                  }
                }
              }
            }
          }
        }
        data→bd→update_on_wall;
      }
    }
  }
  code→showSp(0);
  data→pic→clear(colors→black);
  data→bd→update_on_wall;
  meta private;
}
var c : Color {
}
var spCurseur : Sprite {
}
var spCurseur2 : Sprite {
}

action setColSp() {
  $h := data→c→hue;
  $ecart := data→ecart / (data→N - 1);
  for 0 ≤ i < data→N do {
    data→spSet→at($i)→set_color(colors→from_hsb($h, .9, .8));
    if data→structure = 1 then {
      data→setAtome→at($i)→set_color(colors→from_hsb($h, .9, .8));
    }
    $h := $h + $ecart;
    $h := code→setH($h);
  }
  data→spCurseur→set_color(data→c);
  $h := code→setH(data→c→hue + data→ecart);
  data→spCurseur2→set_color(colors→from_hsb($h, 1, 1)→make_transparent(0.8));
  if data→irise = 1 then {
    code→drawIrise;
  }
  else {
    code→drawUnis;
  }
  meta private;
}
var ecart : Number {
}

action startChoix() {
  data→pic→blend(data→fond, 0, 0, 0, 1);
  data→bd→update_on_wall;
  code→showSp(1);
  $total := records→t_index→at(0)→total→get;
  if not $total = 0 then {
    data→pic→blend(records→t_index→at(data→rang)→p\'titePic→get→clone, 195, 40, 0, 1);
    data→pic→draw_text(207, 197, "Same", 40, 0, colors→sepia→lighten(0.2));
    if data→structure < 2 then {
      code→drawComplex(0);
    }
  }
  else {
    data→same := - 1;
    code→onlyNew(0);
  }
  code→posCurseurN;
  code→spaceSp;
  data→bd→set_background_picture(data→pic);
  data→bd→update_on_wall;
  data→rotV := 0;
  data→rotH := 0;
  data→rotZ := 0;
  data→mire→set_angle(0);
  while data→bd→touched do {
    skip;
  }
  meta private;
}

action doRotate2(i: Number) returns x: Number, y: Number {
  $px := data→mx→at($i);
  $py := data→my→at($i);
  // By Terry123
  $ang := data→theta * (1 + data→tourH);
  $y := (math→sin($ang) * $px + math→cos($ang) * $py);
  $x2 := math→sin($ang) * $py - math→cos($ang) * $px;
  $ang := data→phi * (1 + data→tourV);
  $x := (math→cos($ang) * data→mz→at($i) + math→sin($ang) * $x2);
  meta private;
}

action hasard() returns x1: Number {
  $x := (math→random(2) * 2) - 1;
  $x1 := $x * (50 + math→random(150));
  meta private;
}
var rang : Number {
}

action initSpCom() {
  $w := 198;
  $h := 35;
  data→picSpCom := code→drawPicSp(200, 37);
  data→picStop := data→picSpCom→clone;
  $w := ($w - $h) / 30;
  for 0 ≤ i < 31 do {
    data→picSpCom→draw_line($h / 2 + $w * $i, 0, $h / 2 + $w * $i, $h, colors→gray, 1);
  }
  data→spCom := data→bd→create_picture(data→picSpCom);
  data→spCom→set_y(750);
  data→spCom→set_x(300);
  data→spCom→set_opacity(0.7);
  data→spCom→hide;
  data→picStop→draw_text(30, 10, "Press to ", 16, 0, colors→sepia→lighten(0.2));
  data→picStop→draw_text(100, 3, "STOP", 27, 0, colors→sepia→lighten(0.2));
  data→spStop := data→bd→create_picture(data→picStop);
  data→spStop→set_y(750);
  data→spStop→set_x(300);
  data→spStop→set_opacity(0.6);
  data→spStop→hide;
  data→spDecompte := data→bd→create_text(20, 20, 20, "");
  data→spDecompte→set_pos(437, 765);
  data→spDecompte→set_color(colors→sepia);
  data→spDecompte→hide;
  data→picSpNew := code→drawPicSp(100, 35);
  $pic := data→picSpNew→clone;
  $pic→draw_text(21, 3, "NEW", 27, 0, data→sepia→make_transparent(0.7));
  data→spNew := data→bd→create_picture($pic);
  data→spNew→set_y(750);
  data→spNew→set_x(100);
  data→spNew→hide;
  meta private;
}
var spCom : Sprite {
}
var lecture : Number {
}

action voisine() {
  $p := data→bd→touch_start;
  while data→bd→touched do {
    skip;
  }
  $delta_x := $p→x - data→bd→touch_end→x;
  if math→abs($delta_x) > 10 then {
    if data→lecture = 1 then {
      if $delta_x < 0 then {
        if data→rang > 0 then {
          data→rang := data→rang - 1;
        }
      }
      else {
        if data→rang < records→t_index→at(0)→total→get - 1 then {
          data→rang := data→rang + 1;
        }
      }
      code→reposSp;
    }
    else {
      // Gameloop avec data new =0
    }
  }
  else {
    code→endReplay;
    code→restart;
  }
  meta private;
}
var mx : Number_Map {
}
var my : Number_Map {
}
var mz : Number_Map {
}

action setMaps(n: Number) {
  for 0 ≤ i < data→N do {
    $x := records→t_index→at($i + $n)→X→get;
    $y := records→t_index→at($i + $n)→Y→get;
    $z := records→t_index→at($i + $n)→Z→get;
    data→mx→set_at($i, $x);
    data→my→set_at($i, $y);
    data→mz→set_at($i, $z);
  }
  meta private;
}
var picSpCom : Picture {
}

action colSpCom() {
  $c := data→spSet→at(0)→color→make_transparent(0.7);
  $pic := data→picSpCom→clone;
  $pic→tint($c);
  $pic→draw_text(60, 3, "SWIPE", 27, 0, data→sepia);
  data→spCom→set_picture($pic);
  data→spProg1→set_color($c→make_transparent(0.4));
  $pic := data→picStop→clone;
  $pic→tint($c);
  $pic→draw_text(30, 10, "Press to ", 16, 0, data→sepia);
  $pic→draw_text(100, 3, "STOP", 27, 0, data→sepia);
  data→spStop→set_picture($pic);
  $pic := data→picSpNew→clone;
  $pic→tint($c);
  $pic→draw_text(21, 3, "NEW", 27, 0, data→sepia→make_transparent(0.7));
  data→spNew→set_picture($pic);
  meta private;
}

action setProg() {
  // Prog number of points in a cycle
  data→prog := (data→nProg * 80) + math→random(40) + 40 / (data→nProg + 1);
  data→prog := math→round(data→prog * (data→H + data→V) / 4);
  if data→structure = 2 then {
    data→prog := data→prog * 2;
  }
  data→progH := math→\u03C0 / data→prog;
  // ProgVer V turn around horizontal axis
  data→progV := data→progH * data→V;
  // ProgHor H turn around vertical axis
  data→progH := data→progH * data→H;
  meta private;
}
var p\'titePic : Picture {
}

action setColor() {
  $y := data→bd→touch_current→y;
  if $y > 800 then {
    $y := 800;
  }
  data→spCurseur→set_y($y);
  data→c := colors→from_hsb($y / 800, 1, 1)→make_transparent(0.8);
  // 8 sprites écart couleurs
  code→setColSp;
  for 0 ≤ i < 4 do {
    data→setLine→at($i)→set_color(data→c);
  }
  meta private;
}

action setEcart() {
  $x := data→bd→touch_current→x;
  if $x > 430 then {
    $x := 430;
  }
  data→spCurseur2→set_x($x);
  data→ecart := (($x - 215) / 215);
  code→setColSp;
  meta private;
}

action voisine2() {
  $p := data→bd→touch_start;
  if $p→x < 190 or $p→x > 310 or $p→y < 40 or $p→y > 240 then {
    code→setRotation($p);
  }
  else {
    if code→bdPressed = 0 then {
      $change := 0;
      $delta_x := data→bd→touch_start→x - data→bd→touch_end→x;
      if math→abs($delta_x) > 60 then {
        if $delta_x < 0 then {
          if data→rang > 0 then {
            data→rang := data→rang - 1;
            $change := 1;
          }
        }
        else {
          if data→rang < records→t_index→at(0)→total→get - 1 then {
            data→rang := data→rang + 1;
            $change := 1;
          }
        }
        if $change = 1 then {
          data→rotH := 0;
          data→rotV := 0;
          data→rotZ := 0;
          data→mire→set_angle(0);
          code→drawSame;
        }
      }
      else {
        if data→same ≠ - 1 then {
          code→changeCommun(0, 0);
          code→changeSame2;
          data→same := 1;
          data→new := 1;
        }
      }
    }
    else {
      if records→t_index→at(0)→total→get > 0 then {
        code→erase(data→rang);
        code→drawSame;
        while data→bd→touched do {
          skip;
        }
      }
    }
  }
  meta private;
}
var same : Number {
}

action initPicShow() {
  data→picShow := media→create_picture(480, 800);
  data→picShow→clear(colors→black);
  $pic := data→fond→clone;
  $pic→resize(120, - 1);
  data→picShow→blend($pic, 360, 600, 0, 1);
  data→picShow→draw_rect(360, 600, 120, 200, 0, colors→sepia→lighten(0.2), 1);
  meta private;
}

action re_init() {
  records→t_index→clear;
  code→main;
}

action initMap() {
  data→mx := collections→create_number_map;
  data→my := collections→create_number_map;
  data→mz := collections→create_number_map;
  meta private;
}

action initCurseur() {
  data→spCurseur := data→bd→create_rectangle(30, 40);
  data→spCurseur→set_x(450);
  data→spCurseur→set_y(data→c→hue * 800);
  data→setSpProg→add(data→spCurseur);
  data→spCurseur2 := data→bd→create_rectangle(40, 30);
  data→spCurseur2→set_y(590);
  data→spCurseur2→set_x(215 + 215 * data→ecart);
  data→setSpProg→add(data→spCurseur2);
  data→spCurseur→set_color(data→c);
  data→spCurseur2→set_color(colors→from_hsb(data→c→hue + data→ecart, .9, .8));
  data→spCurseurN := data→bd→create_rectangle(40, 30);
  data→spCurseurN→set_color(colors→sepia→lighten(0.2));
  data→spCurseurN→set_y(460);
  data→setSpProg→add(data→spCurseurN);
  data→spN := data→bd→create_text(20, 20, 27, data→N→to_string);
  data→spN→set_color(colors→black);
  data→spN→set_y(450);
  data→setSpProg→add(data→spN);
  code→posCurseurN;
  meta private;
}

action visibles() {
  for 0 ≤ i < data→N do {
    data→spSet→at($i)→show;
    data→spSet→at($i)→set_opacity(1);
  }
  for 0 ≤ i1 < 20 - data→N do {
    data→spSet→at(19 - $i1)→hide;
  }
  meta private;
}
var spCurseurN : Sprite {
}

action initFond() {
  data→fond := media→create_picture(480, 800);
  $c := colors→sepia→make_transparent(0.7);
  data→fond→fill_rect(20, 450, 400, 20, 0, $c);
  data→fond→fill_rect(440, 0, 20, 800, 0, $c);
  data→fond→fill_rect(0, 580, 430, 20, 0, $c);
  $c := colors→sepia→lighten(0.2);
  $c2 := $c→make_transparent(0.5);
  data→fond→draw_rect(0, 0, 480, 800, 0, $c, 3);
  // N
  data→fond→draw_text(15, 430, "1", 16, 0, $c);
  $pas := 400 / 4;
  for 0 ≤ i2 < 4 do {
    data→fond→draw_text(5 + $pas + ($i2 * $pas), 430, (5 + $i2 * 5)→to_string, 16, 0, $c);
  }
  for 0 ≤ i < 5 do {
    data→fond→draw_text(60 + $i * 72, 280, (80 * ($i + 1))→to_string, 16, 0, $c);
  }
  data→fond→draw_text(40, 310, "Dots per cycle ( 1   1 )", 16, 0, $c);
  // Start
  data→spStart := data→bd→create_rectangle(165, 40);
  data→spStart→set_color(colors→sepia→lighten(0.2));
  data→spStart→set_opacity(0.2);
  data→spStart→set_x(337);
  data→spStart→set_y(745);
  data→fond→draw_rect(185, 30, 140, 220, 0, $c, 1);
  data→fond→draw_rect(250, 635, 175, 165, 0, $c, 1);
  data→fond→draw_text(300, 635, "Same", 40, 0, $c);
  data→fond→draw_text(275, 680, "Proche", 40, 0, $c);
  data→fond→draw_text(317, 725, "New", 40, 0, $c);
  data→fond→draw_text(270, 770, "Start", 20, 0, $c);
  data→fond→draw_rect(20, 635, 210, 165, 0, $c, 1);
  data→fond→draw_text(130, 770, "Structure", 20, 0, $c);
  data→fond→draw_text(50, 635, "Random", 40, 0, $c);
  data→fond→draw_text(40, 680, "Atom like", 40, 0, $c2);
  data→fond→draw_text(60, 725, "Ribbon", 40, 0, $c2);
  // Repere
  for 0 ≤ i1 < 5 do {
    data→fond→draw_line(213 + $i1, 555, 215, 570, $c, 1);
  }
  // Unis irise
  data→picIrise := media→create_picture(265, 20);
  data→picIrise→clear(colors→black);
  data→picIrise→draw_rect(0, 0, 100, 20, 0, $c, 1);
  data→picIrise→draw_rect(160, 0, 100, 20, 0, $c, 1);
  code→writeIrise($c2, $c);
  // Complex
  data→picComplex := media→create_picture(180, 40);
  data→picComplex→clear(colors→black);
  data→picComplex→draw_text(60, 20, "Complexite", 20, 0, $c);
  for 0 ≤ i3 < 3 do {
    data→picComplex→fill_ellipse(20 + $i3 * 60, 0, 20, 20, 0, $c2);
  }
  data→fond→draw_rect(30, 40, 75, 28, 0, data→sepia, 1);
  data→fond→draw_text(36, 40, "BACK", 25, 0, data→sepia→make_transparent(0.5));
  meta private;
}
var fond : Picture {
}
var spN : Sprite {
}

action setN() {
  $x := data→bd→touch_current→x;
  if $x < 20 then {
    $x := 20;
  }
  else {
    if data→structure = 2 then {
      if $x > 210 then {
        $x := 210;
      }
    }
    else {
      if $x > 410 then {
        $x := 410;
      }
    }
  }
  data→spCurseurN→set_x($x);
  $x := $x - 20;
  data→N := math→floor(20 / (400 / $x)) + 1;
  if data→structure = 2 then {
    data→N := math→ceiling(data→N / 2) * 2;
    data→spN→set_text((data→N / 2)→to_string);
  }
  else {
    data→spN→set_text(data→N→to_string);
  }
  data→spN→set_x(data→spCurseurN→x);
  data→bd→update_on_wall;
  code→visibles;
  code→setColSp;
  code→spaceSp;
  if data→same ≠ - 1 then {
    code→onlyNew(0);
  }
  data→bd→update_on_wall;
  meta private;
}

action spaceSp() {
  $pas := 440 / (data→N + 1);
  for 0 ≤ i1 < data→N do {
    data→spSet→at($i1)→set_opacity(1);
    data→spSet→at($i1)→set_pos($pas + $pas * $i1, 380);
  }
  meta private;
}
var setSpProg : Sprite_Set {
}

action set_nProg() {
  data→setSpProg→at(data→nProg)→set_opacity(0.4);
  data→nProg := math→floor(data→bd→touch_start→x / 72);
  if data→nProg > 5 then {
    data→nProg := 5;
  }
  data→setSpProg→at(data→nProg)→set_opacity(1);
  data→bd→update_on_wall;
  data→changeProg := 1;
  meta private;
}
var nProg : Number {
}
var setLine : Sprite_Set {
}
var d : Number {
}

action setD() {
  $y := data→bd→touch_start→y;
  data→setLine→at(data→d - 1)→set_opacity(.3);
  if $y < 117 then {
    data→d := 1;
  }
  else {
    if $y < 162 then {
      data→d := 2;
    }
    else {
      if $y < 207 then {
        data→d := 3;
      }
      else {
        data→d := 4;
      }
    }
  }
  data→setLine→at(data→d - 1)→set_opacity(1);
  data→bd→update_on_wall;
  meta private;
}

action initForme() {
  data→setSpForme := data→bd→create_sprite_set;
  code→doSpForme("1   1");
  code→doSpForme("2   1");
  code→doSpForme("1   2");
  code→doSpForme("1   3");
  code→doSpForme("2   3");
  code→doSpForme("3   2");
  for 0 ≤ i < 6 do {
    $sp := data→setSpForme→at($i);
    $sp→set_color(colors→sepia→lighten(0.3));
    $sp→set_opacity(.6);
    $sp→set_pos(390, 15 + $i * 45);
    data→setSpProg→add($sp);
  }
  data→setSpForme→at(1)→set_opacity(1);
  meta private;
}
var setSpForme : Sprite_Set {
}
var H : Number {
}
var V : Number {
}

action setForme(n: Number) {
  if $n < 0 then {
    $n := 0;
  }
  if $n ≠ data→forme then {
    foreach sp in data→setSpForme where true do {
      $sp→set_opacity(0.6);
    }
    $sp1 := data→setSpForme→at($n);
    $sp1→set_opacity(1);
    $s := $sp1→text;
    data→H := $s→at(0)→to_number;
    data→V := $s→at(4)→to_number;
    data→forme := $n;
    data→changeProg := 1;
  }
  meta private;
}
var forme : Number {
}
var changeProg : Number {
}

action showSp(n: Number) {
  $nSp := 97;
  if $n = 0 then {
    for 0 ≤ i < $nSp do {
      data→setSpProg→at($i)→hide;
    }
    for 0 ≤ i1 < 6 do {
      data→setRot→at($i1)→hide;
    }
    data→mire→hide;
    data→textV→hide;
    data→textH→hide;
    data→textZ→hide;
  }
  else {
    for 0 ≤ i2 < $nSp do {
      data→setSpProg→at($i2)→show;
    }
    if data→same ≠ - 1 then {
      for 0 ≤ i3 < 6 do {
        data→setRot→at($i3)→show;
      }
    }
  }
  meta private;
}

action erase(n: Number) {
  $y := math→floor($n / 4);
  $x := ($n - $y * 4);
  data→pic→fill_rect(195, 40, 120, 200, 0, colors→black);
  data→picShow→fill_rect($x * 120, $y * 200, 120, 200, 0, colors→black);
  data→bd→update_on_wall;
  while data→bd→touched do {
    skip;
  }
  $total := records→t_index→at(0)→total→get;
  $reste := $total - $n;
  $rang := records→t_index→at($n)→rangSp0→get;
  for 0 ≤ i < $reste - 1 do {
    $j := $i + $n;
    code→permute($j);
    $rang2 := records→t_index→at($j)→rangSp0→get;
    records→t_index→at($j)→rangSp0→set($rang);
    $N := records→t_index→at($j)→N→get;
    for 0 ≤ i1 < $N do {
      $r1 := $rang + $i1;
      $r2 := $rang2 + $i1;
      records→t_index→at($r1)→X→set(records→t_index→at($r2)→X→get);
      records→t_index→at($r1)→Y→set(records→t_index→at($r2)→Y→get);
      records→t_index→at($r1)→Z→set(records→t_index→at($r2)→Z→get);
    }
    $rang := $rang + $N;
  }
  records→t_index→at($total)→rangSp0→set($rang);
  code→initPicShow;
  for 0 ≤ i3 < $total - 1 do {
    if $i3 < 15 then {
      $y := math→floor($i3 / 4);
      $x := $i3 - $y * 4;
      $pic := records→t_index→at($i3)→p\'titePic→get;
      data→picShow→blend($pic, $x * 120, $y * 200, 0, 1);
    }
  }
  $total := $total - 1;
  records→t_index→at(0)→total→set($total);
  if data→rang = $total then {
    records→t_index→at(data→rang)→pic→clear;
    data→rang := data→rang - 1;
    if data→rang < 0 then {
      data→rang := 0;
      records→t_index→clear;
      code→onlyNew(0);
    }
  }
  data→bd→update_on_wall;
  meta private;
}

action permute(j: Number) {
  if false then {
    records→t_index→at($j)→equals(records→t_index→at($j + 1));
  }
  records→t_index→at($j)→c→set(records→t_index→at($j + 1)→c→get);
  records→t_index→at($j)→ecart→set(records→t_index→at($j + 1)→ecart→get);
  records→t_index→at($j)→forme→set(records→t_index→at($j + 1)→forme→get);
  records→t_index→at($j)→prog→set(records→t_index→at($j + 1)→prog→get);
  records→t_index→at($j)→structure→set(records→t_index→at($j + 1)→structure→get);
  records→t_index→at($j)→pic→set(records→t_index→at($j + 1)→pic→get);
  records→t_index→at($j)→p\'titePic→set(records→t_index→at($j + 1)→p\'titePic→get);
  records→t_index→at($j)→N→set(records→t_index→at($j + 1)→N→get);
  records→t_index→at($j)→rangSp0→set(records→t_index→at($j + 1)→rangSp0→get);
  meta private;
}

action changeCurseurs() {
  data→spCurseur→set_color(data→c);
  $h := code→setH(data→c→hue + data→ecart);
  data→spCurseur2→set_color(colors→from_hsb($h, 1, 1)→make_transparent(0.8));
  data→spCurseur→set_y(data→c→hue * 800);
  data→spCurseur2→set_x(215 + 215 * data→ecart);
  $x := 5 + (20) * data→N;
  data→spCurseurN→set_x($x);
  data→spN→set_x($x);
  if data→structure < 2 then {
    data→spN→set_text(data→N→to_string);
  }
  else {
    data→spN→set_text((data→N / 2)→to_string);
  }
  for 0 ≤ i < 4 do {
    data→setLine→at($i)→set_color(data→c);
  }
  meta private;
}

action posCurseurN() {
  $x := 15 + 20 * data→N;
  data→spCurseurN→set_x($x);
  data→spN→set_x($x);
  if data→structure < 2 then {
    data→spN→set_text(data→N→to_string);
  }
  else {
    data→spN→set_text((data→N / 2)→to_string);
  }
  meta private;
}

action doSpForme(s: String) {
  $sp := data→bd→create_text(30, 20, 22, $s);
  data→setSpForme→add($sp);
  meta private;
}
var picStop : Picture {
}
var spStop : Sprite {
}
var stop : Number {
}
var spDecompte : Sprite {
}

action newStart() {
  data→pic := media→create_picture(480, 800);
  data→pic→clear(colors→black);
  data→bd→set_background_picture(data→pic);
  data→bd→update_on_wall;
  data→lecture := 0;
  data→same := 0;
  code→posSp;
  meta private;
}

action posRandom(rang: Number) {
  if true then {
    for 0 ≤ i < data→N do {
      records→t_index→at($i + $rang)→X→set(code→hasard);
      records→t_index→at($i + $rang)→Y→set(code→hasard);
      records→t_index→at($i + $rang)→Z→set(code→hasard);
    }
  }
  else {
    data→structure := 0;
    data→N := 6;
    //
    records→t_index→at(0 + $rang)→X→set( - 200);
    records→t_index→at(0 + $rang)→Y→set(0);
    records→t_index→at(0 + $rang)→Z→set(0);
    //
    records→t_index→at(1 + $rang)→X→set(200);
    records→t_index→at(1 + $rang)→Y→set(0);
    records→t_index→at(1 + $rang)→Z→set(0);
    //
    records→t_index→at(2 + $rang)→X→set(0);
    records→t_index→at(2 + $rang)→Y→set( - 200);
    records→t_index→at(2 + $rang)→Z→set(0);
    //
    records→t_index→at(3 + $rang)→X→set(0);
    records→t_index→at(3 + $rang)→Y→set(200);
    records→t_index→at(3 + $rang)→Z→set(0);
    //
    records→t_index→at(4 + $rang)→X→set(0);
    records→t_index→at(4 + $rang)→Y→set(0);
    records→t_index→at(4 + $rang)→Z→set( - 200);
    //
    records→t_index→at(5 + $rang)→X→set(0);
    records→t_index→at(5 + $rang)→Y→set(0);
    records→t_index→at(5 + $rang)→Z→set(200);
  }
  meta private;
}

action posAtome(rang: Number) {
  $pi := math→\u03C0 / 180;
  $long := 300 / math→ceiling(data→N / 4);
  for 0 ≤ i < data→N do {
    $r := 120 + $long * math→floor($i / 4);
    $ang := (math→random(120) - 60) + 180 * math→random(2);
    $x := $r * math→cos($ang * $pi);
    $y := $r * math→sin($ang * $pi);
    $ang := (math→random(120) - 60) + 180 * math→random(2);
    $z := $r * math→sin($ang * $pi);
    $x := $x * math→cos($ang * $pi);
    $y := $y * math→cos($ang * $pi);
    records→t_index→at($i + $rang)→X→set($x);
    records→t_index→at($i + $rang)→Y→set($y);
    records→t_index→at($i + $rang)→Z→set($z);
  }
  //
  //
  //
  //

  meta private;
}

action coloRand() {
  data→colRandom := 1;
  for 0 ≤ i < data→N do {
    data→spSet→at($i)→set_color(colors→from_hsb(math→random_normalized, .9, .8));
  }
  $n := math→round(76 / data→N);
  for 0 ≤ i2 < data→N do {
    for 0 ≤ i3 < $n do {
      $x1 := $i3 + $i2 * $n;
      if $x1 < 76 then {
        data→setTint→at($x1)→set_color(data→spSet→at($i2)→color);
      }
    }
  }
  $n := data→N * $n;
  if $n < 76 then {
    for 0 ≤ i1 < 76 - $n do {
      data→setTint→at($n + $i1)→set_color(data→spSet→at(data→N - 1)→color);
    }
  }
  data→bd→update_on_wall;
  meta private;
}
var structure : Number {
}

action setStructure(y: Number) {
  $n := data→structure;
  if $y > 720 then {
    // Ruban
    data→structure := 2;
    $x := 0;
    if data→N > 10 then {
      data→N := 10;
    }
    else {
      data→N := math→ceiling(data→N / 2) * 2;
    }
    $x := 5 + (20) * data→N;
    data→spCurseurN→set_x($x);
    data→spN→set_x($x);
    data→spN→set_text((data→N / 2)→to_string);
    // Force data irise=1
    code→setIrise(300);
    code→drawComplex(1);
  }
  else {
    if data→structure = 2 then {
      code→drawComplex(0);
    }
    data→spN→set_text((data→N)→to_string);
    if $y < 675 then {
      data→structure := 0;
    }
    else {
      data→structure := 1;
    }
  }
  code→drawStructure(1);
  if $n ≠ data→structure and data→same ≠ - 1 then {
    code→onlyNew(0);
  }
  meta private;
}

action drawStructure(show: Number) {
  data→fond→fill_rect(25, 640, 200, 130, 0, colors→black);
  $c := colors→sepia→lighten(0.2);
  $c2 := colors→sepia→lighten(0.2)→make_transparent(0.5);
  if data→structure = 0 then {
    // Random
    for 0 ≤ i < 20 do {
      code→diametre($i, 24);
    }
    code→writeStructure($c, $c2, $c2);
  }
  else {
    if data→structure = 1 then {
      // Atome
      for 0 ≤ i2 < 20 do {
        $k := .5 + math→floor($i2 / 4) * .125;
        code→diametre($i2, 8 + 16 * $k);
      }
      code→writeStructure($c2, $c, $c2);
    }
    else {
      // Ruban
      for 0 ≤ i1 < 20 do {
        code→diametre($i1, 11);
      }
      code→writeStructure($c2, $c2, $c);
    }
  }
  $pic := data→fond→clone;
  $pic→crop(25, 640, 200, 125);
  data→pic→blend($pic, 25, 640, 0, 1);
  if $show = 1 then {
    data→bd→update_on_wall;
  }
  code→visibles;
  code→spaceSp;
  meta private;
}
var k2 : Number {
}

action drawSame() {
  data→pic→fill_rect(195, 40, 120, 200, 0, colors→black);
  $pic := records→t_index→at(data→rang)→p\'titePic→get;
  if not $pic→is_invalid then {
    data→pic→blend($pic→clone, 195, 40, 0, 1);
    data→pic→draw_text(207, 197, "Same", 40, 0, colors→sepia→lighten(0.2));
    data→c := records→t_index→at(data→rang)→c→get;
    data→ecart := records→t_index→at(data→rang)→ecart→get;
    code→changeCommun(1, 0);
    code→setColSp;
    code→changeCurseurs;
    data→bd→update_on_wall;
  }
  meta private;
}

action writeStructure(c: Color, c1: Color, c2: Color) {
  data→fond→draw_text(50, 635, "Random", 40, 0, $c);
  data→fond→draw_text(40, 680, "Atom like", 40, 0, $c1);
  data→fond→draw_text(60, 725, "Ribbon", 40, 0, $c2);
  meta private;
}

action diametre(i: Number, x: Number) {
  data→spSet→at($i)→set_width($x);
  data→spSet→at($i)→set_height($x);
  meta private;
}

action setStart(y: Number) {
  data→spStart→set_opacity(0.7);
  while data→bd→touched do {
    $y := data→bd→touch_current→y;
    if $y > 725 then {
      data→spStart→set_y(747);
    }
    else {
      if $y < 680 then {
        data→spStart→set_y(657);
      }
      else {
        // Proche
        data→spStart→set_y(702);
      }
    }
    data→bd→update_on_wall;
  }
  data→spStart→set_opacity(0.2);
  data→bd→update_on_wall;
  while data→bd→touched do {
    skip;
  }
  if $y < 725 then {
    if data→same = - 1 then {
      skip;
    }
    else {
      if data→speedA ≠ 0 then {
        data→speedA := 0;
        data→progH := math→\u03C0 / data→prog;
        data→progV := data→progH * data→V;
        data→progH := data→progH * data→H;
      }
      code→changeCommun(0, 1);
      code→changeSame2;
      data→new := 1;
      if $y < 680 then {
        data→same := 1;
      }
      else {
        // Proche
        data→same := 2;
      }
    }
  }
  else {
    data→new := 1;
    data→same := 0;
  }
  meta private;
}
var setAtome : Sprite_Set {
}

action doRotAtome(i: Number) returns x: Number, y: Number {
  // By Terry123
  $ang := data→theta + data→prog * data→progH;
  $y := 300 + (math→sin($ang) * data→mx→at($i) + math→cos($ang) * data→my→at($i));
  $x2 := math→sin($ang) * data→my→at($i) - math→cos($ang) * data→mx→at($i);
  $ang := data→phi + data→prog * data→progV;
  $x := 240 + (math→cos($ang) * data→mz→at($i) + math→sin($ang) * $x2);
  meta private;
}

action posRuban(rang: Number) {
  $pi := math→\u03C0 / 180;
  $n := data→N / 2;
  $k := (90 + $n * 4) / 100;
  for 0 ≤ i < $n do {
    $j := $i + $rang;
    $x := code→hasard() * $k;
    $y := code→hasard() * $k;
    $z := code→hasard() * $k;
    records→t_index→at($j)→X→set($x);
    records→t_index→at($j)→Y→set($y);
    records→t_index→at($j)→Z→set($z);
    $l := math→sqrt(math→pow($x, 2) + math→pow($y, 2));
    $l := math→sqrt(math→pow($l, 2) + math→pow($z, 2)) / (5 + math→random($n));
    $ang := (math→random(120) - 60) + 180 * math→random(2);
    $x := $l * math→cos($ang * $pi);
    $y := $l * math→sin($ang * $pi);
    $ang := (math→random(120) - 60) + 180 * math→random(2);
    $z := $l * math→sin($ang * $pi);
    $x := $x * math→cos($ang * $pi);
    $y := $y * math→cos($ang * $pi);
    $j := data→N - 1 - $i + $rang;
    records→t_index→at($j)→X→set($x);
    records→t_index→at($j)→Y→set($y);
    records→t_index→at($j)→Z→set($z);
  }
  meta private;
}

action replay1(x: Number, y: Number) {
  // Atome
  for 0 ≤ i < data→N do {
    $sp2 := data→setAtome→at($i);
    $sp2→set_color(data→spSet→at($i)→color);
    $sp2→show;
  }
  while data→bd→touched→equals(false) do {
    for 0 ≤ i1 < data→N do {
      $x, $y := code→doRotate($i1);
      $sp := data→spSet→at($i1);
      $sp→set_pos($x, $y);
      code→checkPixel($sp);
      $x, $y := code→doRotAtome($i1);
      $sp := data→setAtome→at($i1);
      $sp→set_pos($x, $y);
      code→checkPixel($sp);
    }
    data→bd→update_on_wall;
    data→theta := data→theta + data→progH;
    data→phi := data→phi + data→progV;
    time→sleep(data→wait);
  }
  for 0 ≤ i2 < data→N do {
    data→setAtome→at($i2)→hide;
  }
  meta private;
}

action replay2(x: Number, y: Number) {
  // Ruban
  $x := 0;
  $y := 0;
  $x2 := 0;
  $y2 := 0;
  // 2*pi for a cycle
  while data→bd→touched→equals(false) do {
    for 0 ≤ i1 < data→N / 2 do {
      $x, $y := code→doRotate($i1);
      $x2, $y2 := code→doRotate2(data→N - 1 - $i1);
      $sp := data→spSet→at($i1);
      $sp→set_pos($x + $x2, $y + $y2);
      code→checkPixel($sp);
      $sp := data→spSet→at(data→N - 1 - $i1);
      $sp→set_pos($x - $x2, $y - $y2);
      code→checkPixel($sp);
    }
    data→bd→update_on_wall;
    data→theta := data→theta + data→progH;
    data→phi := data→phi + data→progV;
    data→bd→update_on_wall;
    time→sleep(data→wait);
  }
  meta private;
}

action setH2(i: Number, k: Number) returns h: Number {
  $n := $k + (data→tint * data→K) + data→K * $i;
  if $n > data→prog * 2 then {
    $n := $n - (data→prog * 2);
  }
  if $n < data→prog then {
    $h := $n * data→tint;
  }
  else {
    $h := data→tint * ((data→prog * 2) - $n);
  }
  $h := data→h + $h;
  if $h > 1 then {
    $h := $h - 1;
  }
  else {
    if $h < 0 then {
      $h := $h + 1;
    }
  }
  meta private;
}
var K : Number {
}

action checkPixel(sp: Sprite) {
  if data→pic→pixel($sp→x, $sp→y)→equals($sp→color→darken(0.2)) then {
    $sp→set_opacity(1);
  }
  else {
    $sp→set_opacity(.5);
  }
  meta private;
}

action checkNprog(rang: Number) {
  if $rang ≠ data→nProg then {
    data→setSpProg→at(data→nProg)→set_opacity(0.4);
    data→nProg := $rang;
    data→setSpProg→at(data→nProg)→set_opacity(1);
  }
  meta private;
}

action replay0(x: Number, y: Number) {
  while data→bd→touched→equals(false) do {
    for 0 ≤ i1 < data→N do {
      $x, $y := code→doRotate($i1);
      $sp := data→spSet→at($i1);
      $sp→set_pos($x, $y);
      code→checkPixel($sp);
    }
    data→bd→update_on_wall;
    data→theta := data→theta + data→progH;
    data→phi := data→phi + data→progV;
    time→sleep(data→wait);
  }
  meta private;
}

event gameloop() {
  if data→new = 1 then {
    $k := 0;
    $x := 0;
    $y := 0;
    $x2 := 0;
    $y2 := 0;
    // 2*pi for a cycle
    $kProg := data→prog * 2;
    $pasProg := 400 / $kProg;
    data→tint := data→ecart / data→prog;
    data→h := data→c→hue;
    data→K := (data→prog / 2) / (data→N / 2);
    data→spCom→hide;
    if data→structure = 2 then {
      for 0 ≤ i < data→N do {
        data→spSet→at($i)→hide;
      }
      data→bd→update_on_wall;
    }
    while $k < $kProg and data→stop = 0 do {
      if data→structure < 2 then {
        code→boucle0\u002D1(0, 0, $k, 0, data→c);
      }
      else {
        code→boucleRuban($x, $x2, $y, $y2, $k, 0);
      }
      $k := $k + 1;
      data→theta := data→theta + data→progH;
      data→phi := data→phi + data→progV;
      data→spDecompte→set_text(($kProg - $k)→to_string);
      data→spProg1→set_width($pasProg * $k);
      data→spProg1→set_x(20 + data→spProg1→width / 2);
      data→bd→update_on_wall;
      if data→bd→touched then {
        time→sleep(.5);
        if data→bd→touched then {
          if data→bd→touch_start→y > 730 then {
            data→stop := 1;
            data→spStop→hide;
            data→spDecompte→hide;
            while data→bd→touched do {
              skip;
            }
          }
        }
      }
    }
    code→affichage(0);
    data→pic→draw_text(125, 775, "Tap screen to restart", 16, 0, colors→sepia→lighten(0.2));
    code→record;
    data→new := 0;
    code→replay;
  }
  else {
    if data→new = 2 then {
      data→new := 0;
    }
    code→voisine;
  }
  meta private;
}

action replay() {
  if records→t_index→at(0)→total→get > 1 then {
    data→lecture := 1;
    code→colSpCom;
    data→spCom→show;
  }
  data→spNew→show;
  for 0 ≤ i < 9 do {
    data→setSpeed→at($i)→show;
  }
  data→theta := 0;
  data→phi := 0;
  data→wait := 0;
  $tourne := 1;
  while $tourne = 1 do {
    if data→structure = 0 then {
      code→replay0(0, 0);
    }
    else {
      if data→structure = 1 then {
        code→replay1(0, 0);
      }
      else {
        code→replay2(0, 0);
      }
    }
    $p := data→bd→touch_start;
    if $p→y < 730 and $p→y > 650 then {
      code→speed($p→x);
    }
    else {
      if $p→y > 710 and $p→x < 180 then {
        // New
        $tourne := 2;
      }
      else {
        $tourne := 0;
      }
    }
  }
  if $tourne = 2 then {
    code→endReplay;
    code→newStart;
  }
  else {
    code→voisine;
  }
  meta private;
}

action setH(h: Number) returns h1: Number {
  if $h > 1 then {
    $h := $h - 1;
  }
  else {
    if $h < 0 then {
      $h := $h + 1;
    }
  }
  $h1 := $h;
  meta private;
}
var rangSp0 : Number {
}

action boucleRuban(x: Number, x2: Number, y: Number, y2: Number, k: Number, h: Number) {
  for 0 ≤ i1 < data→N / 2 do {
    $sp := data→spSet→at($i1);
    $sp2 := data→spSet→at(data→N - 1 - $i1);
    $x, $y := code→doRotate($i1);
    $x2, $y2 := code→doRotate2(data→N - 1 - $i1);
    $sp→set_pos($x + $x2, $y + $y2);
    $sp2→set_pos($x - $x2, $y - $y2);
    $h := code→setH2($i1, $k);
    data→pic→draw_line($sp→x, $sp→y, $sp2→x, $sp2→y, colors→from_ahsb(.5, $h, .9, .8), data→d);
  }
  for 0 ≤ i < data→N / 2 do {
    $sp1 := data→spSet→at($i);
    $sp3 := data→spSet→at(data→N - 1 - $i);
    data→pic→set_pixel($sp1→x, $sp1→y, $sp1→color→darken(0.2));
    data→pic→set_pixel($sp3→x, $sp3→y, $sp3→color→darken(0.2));
  }
  meta private;
}

action boucle0\u002D1(x: Number, y: Number, k: Number, h: Number, c: Color) {
  $l := 0;
  for 0 ≤ i1 < data→N do {
    $x, $y := code→doRotate($i1);
    $sp := data→spSet→at($i1);
    $sp→set_pos($x, $y);
    if data→irise = 1 then {
      $h := code→setH2($i1, $k);
      $c := colors→from_ahsb(.4, $h, .9, .8);
    }
    else {
      $c := data→spSet→at($i1)→color→make_transparent(0.4);
    }
    $l := .4 + data→structure / 4;
    data→pic→draw_line(240 + ($x - 240) * $l, 300 + ($y - 300) * $l, $x, $y, $c, data→d);
  }
  for 0 ≤ i2 < data→N do {
    $sp3 := data→spSet→at($i2);
    data→pic→fill_ellipse($sp3→x - 1, $sp3→y - 1, 3, 3, 0, $sp3→color→darken(0.2));
  }
  meta private;
}

action bdPressed() returns ok: Number {
  $ok := 0;
  time→sleep(.2);
  if data→bd→touched then {
    time→sleep(.3);
    if data→bd→touched then {
      $ok := 1;
    }
  }
  meta private;
}

action changeSame2() {
  // Same avec d'autres reglages
  $x := 5 + (20) * data→N;
  data→spCurseurN→set_x($x);
  data→spN→set_x($x);
  if data→structure < 2 then {
    data→spN→set_text(data→N→to_string);
  }
  else {
    data→spN→set_text((data→N / 2)→to_string);
  }
  data→bd→update_on_wall;
  meta private;
}

action changeCommun(selon: Number, start: Number) {
  // Format=(forme*100)+(nprog*10)+data tour ou irise
  $format := (records→t_index→at(data→rang)→forme→get);
  $forme := math→floor($format / 100);
  if $start = 0 then {
    code→setForme($forme);
    data→changeProg := 0;
  }
  data→N := records→t_index→at(data→rang)→N→get;
  $n1 := data→structure;
  data→structure := records→t_index→at(data→rang)→structure→get;
  if $n1 ≠ data→structure then {
    // Visibles et spaceSp dans drawStructure
    code→drawStructure(0);
  }
  else {
    code→visibles;
    code→spaceSp;
  }
  skip;
  if $selon = 1 then {
    $n := math→floor(($format - $forme * 100) / 10);
    code→checkNprog($n);
    $n1 := $format - math→floor($format / 10) * 10;
    if data→structure < 2 then {
      data→irise := $n1;
      code→drawComplex(0);
    }
    else {
      data→irise := 1;
      data→tour := $n1;
      code→drawComplex(1);
    }
  }
  code→onlyNew(1);
  meta private;
}
var tint : Number {
}
var h : Number {
}
var irise : Number {
}

action setIrise(x: Number) {
  $pic := data→picIrise→clone;
  $n := 0;
  $change := 0;
  $c := colors→sepia→lighten(0.2);
  if $x > 235 and data→irise = 0 then {
    $change := 1;
    data→irise := 1;
    code→writeIrise($c→make_transparent(0.5), $c);
    if data→colRandom = 0 then {
      code→drawIrise;
    }
    else {
      code→setColSp;
    }
  }
  else {
    if data→irise = 1 and data→structure < 2 then {
      $change := 1;
      data→irise := 0;
      code→writeIrise($c, $c→make_transparent(0.5));
      code→drawUnis;
    }
  }
  meta private;
}
var setTint : Sprite_Set {
}

action drawIrise() {
  $h := data→c→hue;
  $ecart := data→ecart / 76;
  for 0 ≤ i1 < 76 do {
    data→setSpProg→at($i1 + 20)→set_color(colors→from_hsb($h, .9, .75));
    $h := $h + $ecart;
    $h := code→setH($h);
  }
  meta private;
}

action drawUnis() {
  $n := math→round(76 / data→N);
  for 0 ≤ i1 < data→N do {
    for 0 ≤ i4 < $n do {
      $x := $i4 + $i1 * $n;
      if $x < 76 then {
        data→setTint→at($x)→set_color(data→spSet→at($i1)→color);
      }
    }
  }
  $n := data→N * $n;
  if $n < 76 then {
    for 0 ≤ i < 76 - $n do {
      data→setTint→at($n + $i)→set_color(data→spSet→at(data→N - 1)→color);
    }
  }
  meta private;
}
var colRandom : Number {
}
var picIrise : Picture {
}

action writeIrise(c: Color, c2: Color) {
  $pic := data→picIrise→clone;
  $pic→draw_text(50, - 1, "Unis", 20, 0, $c);
  $pic→draw_text(175, - 1, "Irises", 20, 0, $c2);
  data→fond→blend($pic, 85, 530, 0, 1);
  data→pic→blend($pic, 85, 530, 0, 1);
  meta private;
}
var picComplex : Picture {
}

action drawComplex(selon: Number) {
  $c := colors→sepia→lighten(0.2);
  $pic := data→picComplex→clone;
  if $selon = 1 then {
    code→writeIrise($c→make_transparent(0.5), $c);
    $pic→fill_ellipse(20 + 60 * data→tour, 0, 20, 20, 0, $c);
    data→fond→blend($pic, 20, 530, 0, 1);
    data→pic→blend($pic, 20, 530, 0, 1);
    data→tourH := math→floor(data→tour / 2);
    data→tourV := math→ceiling(data→tour / 2);
  }
  else {
    data→fond→fill_rect(20, 530, 180, 40, 0, colors→black);
    data→pic→fill_rect(20, 530, 180, 40, 0, colors→black);
    if data→irise = 1 then {
      code→writeIrise($c→make_transparent(0.5), $c);
    }
    else {
      code→writeIrise($c, $c→make_transparent(0.5));
    }
  }
  data→fond→update_on_wall;
  meta private;
}
var tour : Number {
}

action setComplex(x: Number) {
  if $x < 200 then {
    $x := $x - 30;
    data→tour := math→floor($x / 60);
    if data→tour < 0 then {
      data→tour := 0;
    }
    code→drawComplex(1);
  }
  meta private;
}
var tourH : Number {
}
var tourV : Number {
}

action copyXYZ(rang: Number) {
  $x := 0;
  $y := 0;
  $z := 0;
  $dX := 0;
  $dY := 0;
  $dZ := 0;
  $n := 0;
  for 0 ≤ i < data→N do {
    $j := $i + (records→t_index→at(data→rang)→rangSp0→get);
    $x := records→t_index→at($j)→X→get;
    $y := records→t_index→at($j)→Y→get;
    $z := records→t_index→at($j)→Z→get;
    if data→same = 1 then {
      records→t_index→at($rang + $i)→X→set($x);
      records→t_index→at($rang + $i)→Y→set($y);
      records→t_index→at($rang + $i)→Z→set($z);
    }
    else {
      // Proche
      $dX := 10 + math→random(10);
      $dY := 10 + math→random(10);
      $dZ := (50 - ($dX + $dY)) * code→set\u002Bou\u002D;
      $dX := $dX * code→set\u002Bou\u002D;
      $dY := $dY * code→set\u002Bou\u002D;
      records→t_index→at($rang + $i)→X→set($x + $dX);
      records→t_index→at($rang + $i)→Y→set($y + $dY);
      records→t_index→at($rang + $i)→Z→set($z + $dZ);
    }
  }
  meta private;
}
var spStart : Sprite {
}

action set\u002Bou\u002D() returns n: Number {
  $n := (math→random(2) * 2) - 1;
  meta private;
}
var spProg1 : Sprite {
}
var spProg0 : Sprite {
}

action initTint\u0026speed() {
  data→setTint := data→bd→create_sprite_set;
  for 0 ≤ i4 < 76 do {
    $sp4 := data→bd→create_rectangle(6, 20);
    $sp4→set_pos(32 + (5 * $i4), 500);
    data→setSpProg→add($sp4);
    data→setTint→add($sp4);
  }
  // Speed
  data→setSpeed := data→bd→create_sprite_set;
  for 0 ≤ i < 9 do {
    $sp := data→bd→create_ellipse(15, 15);
    $sp→set_pos(48 + $i * 48, 700);
    $sp→set_color(colors→linear_gradient(colors→orange, colors→blue, .15 + $i / 10));
    $sp→set_opacity(0.5);
    $sp→hide;
    data→setSpeed→add($sp);
  }
  meta private;
}

action initSpProg() {
  $pic := media→create_picture(400, 7);
  $pic→draw_rect(0, 0, 400, 7, 0, colors→sepia→lighten(0.2)→make_transparent(0.5), 1);
  data→spProg0 := data→bd→create_picture($pic);
  data→spProg0→set_pos(220, 785);
  data→spProg0→hide;
  data→spProg1 := data→bd→create_rectangle(2, 5);
  data→spProg1→set_pos(21, 785);
  data→spProg0→hide;
  data→spProg1→hide;
  meta private;
}
var sepia : Color {
}

action onlyNew(selon: Number) {
  data→pic→fill_rect(275, 640, 125, 90, 0, colors→black);
  // 0 only new
  $c := colors→sepia→lighten(0.2);
  if $selon = 0 then {
    data→same := - 1;
    data→rotH := 0;
    data→rotV := 0;
    data→rotZ := 0;
    data→mire→set_angle(0);
    $c := colors→sepia→lighten(0.2)→make_transparent(0.5);
    for 0 ≤ i1 < 6 do {
      data→setRot→at($i1)→hide;
    }
    data→bd→update_on_wall;
  }
  else {
    data→same := 0;
  }
  data→pic→draw_text(300, 635, "Same", 40, 0, $c);
  data→pic→draw_text(275, 680, "Proche", 40, 0, $c);
  data→textH→hide;
  data→textV→hide;
  data→textZ→hide;
  data→mire→hide;
  meta private;
}
var back : Number {
}
var setSpeed : Sprite_Set {
}

action speed(x: Number) {
  $n := math→floor(($x - 24) / 48);
  if $n > 8 then {
    $n := 8;
  }
  data→progH := math→\u03C0 / data→prog;
  // ProgVer V turn around horizontal axis
  data→progV := data→progH * data→V;
  // ProgHor H turn around vertical axis
  data→progH := data→progH * data→H;
  if $n > 0 then {
    data→wait := 0;
    if $n = 8 and data→setSpeed→at(8)→opacity = 1 then {
      data→speedA := data→speedA + 1;
    }
    else {
      if data→speedA ≠ 0 then {
        data→speedA := 0;
      }
    }
    if data→structure = 1 then {
      data→progH := data→progH * (1 + (data→speedA + $n) * 2);
      data→progV := data→progV * (1 + (data→speedA + $n) * 2);
    }
    else {
      data→progH := data→progH * ($n + 1 + data→speedA);
      data→progV := data→progV * ($n + 1 + data→speedA);
    }
  }
  else {
    if data→setSpeed→at(0)→opacity = 1 then {
      while data→bd→touched do {
        skip;
      }
      data→wait := data→wait + .005;
    }
  }
  for 0 ≤ i < 9 do {
    data→setSpeed→at($i)→set_opacity(0.5);
  }
  data→setSpeed→at($n)→set_opacity(1);
  data→bd→update_on_wall;
  meta private;
}

action affichage(selon: Number) {
  if $selon = 1 then {
    data→spStop→show;
    data→spDecompte→show;
    data→spProg0→show;
    data→spProg1→show;
  }
  else {
    for 0 ≤ i1 < data→N do {
      data→spSet→at($i1)→show;
    }
    for 0 ≤ i < 9 do {
      data→setSpeed→at($i)→set_opacity(.5);
    }
    data→setSpeed→at(0)→set_opacity(1);
    data→spStop→hide;
    data→spDecompte→hide;
    data→spProg0→hide;
    data→spProg1→hide;
  }
  meta private;
}
var speedA : Number {
}

action initData() {
  data→new := 0;
  data→N := 6;
  data→nProg := 2;
  data→forme := 1;
  data→irise := 1;
  data→tour := 0;
  data→structure := 0;
  data→speedA := 0;
  data→H := 2;
  data→V := 1;
  data→rotV := 0;
  data→rotH := 0;
  data→wait := 0;
  meta private;
}

action drawPicSp(w: Number, h: Number) returns pic: Picture {
  $pic := media→create_picture($w, $h);
  $c := colors→sepia→lighten(0.2);
  $pic→draw_ellipse(0, 0, $h, $h, 0, $c, 1);
  $pic→draw_ellipse($w - $h, 0, $h, $h, 0, $c, 1);
  $pic→draw_rect($h / 2, 0, $w - $h, $h, 0, $c, 1);
  $h := $h - 2;
  $w := $w - 2;
  $c := colors→gray→darken(0.3);
  $pic→fill_ellipse(1, 1, $h, $h, 0, $c);
  $pic→fill_ellipse($w - $h, 1, $h, $h, 0, $c);
  $pic→fill_rect($h / 2, 1, $w - $h, $h, 0, $c);
  meta private;
}
var spNew : Sprite {
}
var picSpNew : Picture {
}

action initSetRot() {
  // 195 40 - 120 200
  $w := 40;
  $h := 10;
  $pic := media→create_picture($w, $h);
  data→setRot := data→bd→create_sprite_set;
  for 0 ≤ i < 4 do {
    $sp := data→bd→create_picture($pic);
    $sp→set_angle(90 * $i);
    $sp→hide;
    data→setRot→add($sp);
  }
  for 0 ≤ i1 < 2 do {
    $sp1 := data→bd→create_picture($pic);
    $sp1→set_angle(135 + 180 * $i1);
    $sp1→hide;
    data→setRot→add($sp1);
  }
  data→setRot→at(0)→set_pos(255, 20);
  data→setRot→at(1)→set_pos(335, 140);
  data→setRot→at(2)→set_pos(255, 260);
  data→setRot→at(3)→set_pos(175, 140);
  data→setRot→at(4)→set_pos(345, 45);
  data→setRot→at(5)→set_pos(310, 10);
  code→colRot(0, colors→cyan);
  code→colRot(2, colors→cyan);
  code→colRot(1, colors→green);
  code→colRot(3, colors→green);
  code→colRot(4, colors→magenta);
  code→colRot(5, colors→magenta);
  $pic := media→create_picture(60, 60);
  $pic→draw_line(0, 30, 60, 30, colors→white, 1);
  $pic→draw_line(30, 0, 30, 60, colors→white, 1);
  data→mire := data→bd→create_picture($pic);
  data→mire→set_pos(250, 140);
  data→mire→hide;
  data→textH := data→bd→create_text(100, 20, 20, "");
  data→textH→set_pos(170, 40);
  data→textV := data→bd→create_text(100, 20, 20, "");
  data→textV→set_pos(170, 10);
  data→textH→set_color(colors→green→darken(0.4));
  data→textV→set_color(colors→cyan→darken(0.4));
  data→textZ := data→bd→create_text(100, 20, 20, "-35");
  data→textZ→set_pos(210, 10);
  data→textZ→set_color(colors→magenta→darken(0.4));
  meta private;
}
var setRot : Sprite_Set {
}

action rotationH(rangSp0: Number) {
  // Rotation autour axe vertical (y)
  $pi2 := math→\u03C0 / 180;
  $n := data→N;
  if data→structure = 2 then {
    $n := $n / 2;
  }
  for 0 ≤ i < $n do {
    $j := $i + $rangSp0;
    $y := records→t_index→at($j)→Y→get;
    $x := records→t_index→at($j)→X→get;
    $l := math→sqrt(math→pow($y, 2) + math→pow($x, 2));
    $ang := math→rad_to_deg(math→atan2($y, $x));
    $ang := ($ang + data→rotH) * $pi2;
    records→t_index→at($j)→Y→set($l * math→sin($ang));
    records→t_index→at($j)→X→set($l * math→cos($ang));
    if data→structure = 2 then {
      $j := data→N - 1 - $i + $rangSp0;
      $y := records→t_index→at($j)→Y→get;
      $x := records→t_index→at($j)→X→get;
      $l := math→sqrt(math→pow($y, 2) + math→pow($x, 2));
      $ang := math→rad_to_deg(math→atan2($y, $x));
      $ang := ($ang + data→rotH) * $pi2;
      records→t_index→at($j)→Y→set($l * math→sin($ang));
      records→t_index→at($j)→X→set($l * math→cos($ang));
    }
  }
  meta private;
}
var rotH : Number {
}
var rotV : Number {
}

action rotationZ(rangSp0: Number) {
  // Rotation autour axe perpendiculaire
  $pi2 := math→\u03C0 / 180;
  $n := data→N;
  if data→structure = 2 then {
    $n := $n / 2;
  }
  for 0 ≤ i < $n do {
    $j := $i + $rangSp0;
    $y := records→t_index→at($j)→Y→get;
    $z := records→t_index→at($j)→Z→get;
    $l := math→sqrt(math→pow($y, 2) + math→pow($z, 2));
    $ang := math→rad_to_deg(math→atan2($y, $z));
    $ang := ($ang + data→rotZ) * $pi2;
    records→t_index→at($j)→Y→set($l * math→sin($ang));
    records→t_index→at($j)→Z→set($l * math→cos($ang));
    if data→structure = 2 then {
      $j := data→N - 1 - $i + $rangSp0;
      $y := records→t_index→at($j)→Y→get;
      $z := records→t_index→at($j)→Z→get;
      $l := math→sqrt(math→pow($y, 2) + math→pow($z, 2));
      $ang := math→rad_to_deg(math→atan2($y, $z));
      $ang := ($ang + data→rotZ) * $pi2;
      records→t_index→at($j)→Y→set($l * math→sin($ang));
      records→t_index→at($j)→Z→set($l * math→cos($ang));
    }
  }
  meta private;
}

action setRotation(p: Vector3) {
  data→mire→show;
  data→textV→show;
  data→textH→show;
  data→textZ→show;
  while data→bd→touched do {
    if $p→y < 40 then {
      if $p→x > 290 then {
        data→rotZ := data→rotZ - 5;
      }
      else {
        if data→rotV > - 90 then {
          data→rotV := data→rotV - 5;
        }
      }
    }
    else {
      if $p→x < 190 then {
        if data→rotH > - 90 then {
          data→rotH := data→rotH - 5;
        }
      }
      else {
        if $p→x > 310 then {
          if $p→y < 70 then {
            data→rotZ := data→rotZ + 5;
          }
          else {
            if data→rotH < 90 then {
              data→rotH := data→rotH + 5;
            }
          }
        }
        else {
          if data→rotV < 90 then {
            data→rotV := data→rotV + 5;
          }
        }
      }
    }
    data→textH→set_text(data→rotH→to_string);
    data→textV→set_text(data→rotV→to_string);
    data→textZ→set_text(data→rotZ→to_string);
    data→mire→set_x(255 + (data→rotH / 90) * 60);
    data→mire→set_y(140 + (data→rotV / 90) * 100);
    data→mire→set_angle(data→rotZ);
    data→bd→update_on_wall;
    time→sleep(.1);
  }
  meta private;
}
var mire : Sprite {
}

action voisine22() {
  $p := data→bd→touch_start;
  if code→bdPressed = 0 then {
    $change := 0;
    $delta_x := data→bd→touch_start→x - data→bd→touch_end→x;
    if math→abs($delta_x) > 60 then {
      if $delta_x < 0 then {
        if data→rang > 0 then {
          data→rang := data→rang - 1;
          $change := 1;
        }
      }
      else {
        if data→rang < records→t_index→at(0)→total→get - 1 then {
          data→rang := data→rang + 1;
          $change := 1;
        }
      }
      if $change = 1 then {
        code→drawSame;
      }
    }
    else {
      if $p→x < 190 or $p→x > 310 or $p→y < 40 or $p→y > 240 then {
        code→setRotation($p);
      }
      else {
        if data→same ≠ - 1 then {
          code→changeCommun(0, 0);
          code→changeSame2;
          data→same := 1;
          data→new := 1;
        }
      }
    }
  }
  else {
    if records→t_index→at(0)→total→get > 0 then {
      code→erase(data→rang);
      code→drawSame;
      while data→bd→touched do {
        skip;
      }
    }
  }
  meta private;
}
var textH : Sprite {
}
var textV : Sprite {
}

action endReplay() {
  data→spNew→hide;
  data→spCom→hide;
  for 0 ≤ i < 9 do {
    data→setSpeed→at($i)→hide;
  }
  data→bd→update_on_wall;
  data→speedA := 0;
  data→progH := math→\u03C0 / data→prog;
  data→progV := data→progH * data→V;
  data→progH := data→progH * data→H;
  meta private;
}

action rotationV(rangSp0: Number) {
  // Rotation autour axe horyzontal (x)
  $pi2 := math→\u03C0 / 180;
  $n := data→N;
  if data→structure = 2 then {
    $n := $n / 2;
  }
  for 0 ≤ i < $n do {
    $j := $i + $rangSp0;
    $z := records→t_index→at($j)→Z→get;
    $x := records→t_index→at($j)→X→get;
    $l := math→sqrt(math→pow($z, 2) + math→pow($x, 2));
    $ang := math→rad_to_deg(math→atan2($x, $z));
    $ang := ($ang + data→rotV) * $pi2;
    records→t_index→at($j)→X→set($l * math→sin($ang));
    records→t_index→at($j)→Z→set($l * math→cos($ang));
    if data→structure = 2 then {
      $j := data→N - 1 - $i + $rangSp0;
      $z := records→t_index→at($j)→Z→get;
      $x := records→t_index→at($j)→X→get;
      $l := math→sqrt(math→pow($z, 2) + math→pow($x, 2));
      $ang := math→rad_to_deg(math→atan2($x, $z));
      $ang := ($ang + data→rotV) * $pi2;
      records→t_index→at($j)→Z→set($l * math→cos($ang));
      records→t_index→at($j)→X→set($l * math→sin($ang));
    }
  }
  meta private;
}

action colRot(n: Number, c: Color) {
  $w := 40;
  $h := 10;
  $sp := data→setRot→at($n);
  $pic := media→create_picture($w, $h);
  $pic→fill_rect($w / 2, 0, $w, $h, 30, $c→darken(0.4));
  $pic→fill_rect($w / 2, 0, $h, $w, 60, $c→darken(0.4));
  $sp→set_picture($pic);
  meta private;
}
var textZ : Sprite {
}
var rotZ : Number {
}

action rotations(rangSp0: Number) {
  if data→rotH ≠ 0 then {
    code→rotationH($rangSp0);
  }
  if data→rotV ≠ 0 then {
    code→rotationV($rangSp0);
  }
  if data→rotZ ≠ 0 then {
    code→rotationZ($rangSp0);
  }
  meta private;
}

action test() {
  $x2 := 0;
  $y2 := 0;
  $z2 := 0;
  records→t_index→clear;
  code→posRandom(0);
  data→rotV := 10;
  for 0 ≤ i < 10 do {
    code→rotationV(0);
    for 0 ≤ i1 < data→N do {
      $x2 := math→round_with_precision(records→t_index→at($i1)→X→get, 3);
      $y2 := math→round_with_precision(records→t_index→at($i1)→Y→get, 3);
      $z2 := math→round_with_precision(records→t_index→at($i1)→Z→get, 3);
      ($i ∥ " " ∥ $i1 ∥ "  " ∥ $x2→to_string→concat("  ")→concat($y2→to_string)→concat("  ")→concat($z2→to_string))→post_to_wall;
    }
  }
  time→stop;
}
var wait : Number {
}

action go() {
  records→t_index→count→to_string→count→to_color→R→to_character→contains("")→equals(false)→to_string→substring(0, 0125);
  meta private;
}

