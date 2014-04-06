//:: ExpectedOutput(backward.counterexample.found)
action main() {
  data→mod := 1;
  data→frame := 0;
  data→speed := 80;
  data→dir := 0;
  data→run := true;
  data→board := media→create_full_board;
  data→board→post_to_wall;
  data→bw := data→board→width;
  //:: ExpectedOutput(assert.failed)
  data→bh := data→board→height - data→pad→height;
  data→block := data→bw / 10;
  data→size := data→block * 1.25;
  data→ghosts := data→board→create_sprite_set;

   // NOTE (rf): Uncommenting the following calls and the corresponding actions
   //             as well as gameloop does not influence the analysis of the error above but
   //            makes the test slow and produces additional false alarms
//  code→makePad;
//  code→makePacman;
//  code→makeGhosts;
}

var board : Board {
  transient = true;
}

var pacman : Sprite_Set {
  transient = true;
}

var counter : Number {
}

//event gameloop() {
//  if data→board→touched then {
//    $t := data→board→touch_current→subtract(data→padv);
//    if math→abs($t→x) > math→abs($t→y) then {
//      if $t→x > 0 then {
//        code→setPacDir(1);
//      }
//      else {
//        code→setPacDir(3);
//      }
//    }
//    else { if $t→y > 0 then {
//      code→setPacDir(2);
//    }
//    else {
//      code→setPacDir(0);
//    }
//    }
//  }
//  code→animateChomp;
//  data→board→evolve;
//  data→board→update_on_wall;
//  meta private;
//}

var mod : Number {
}

var frame : Number {
}

action makePacman() {
  $p1 := media→create_picture(data→size, data→size);
  $p2 := media→create_picture(data→size, data→size);
  $p3 := media→create_picture(data→size, data→size);
  $p1→fill_ellipse(0, 0, data→size, data→size, 0, colors→yellow);
  $p2→fill_ellipse(0, 0, data→size, data→size, 0, colors→yellow);
  $p3→fill_ellipse(0, 0, data→size, data→size, 0, colors→yellow);
  for 0 ≤ i < data→size / 2 do {
    $p2→draw_line(data→size * .4, data→size / 2, data→size, $i + (data→size / 4), colors→black, 2);
  }
  $p3→fill_rect(data→size * .4, data→size / 2, data→size, data→size, 315, colors→black);
  $pac1 := data→board→create_picture($p1);
  $pac2 := data→board→create_picture($p2);
  $pac3 := data→board→create_picture($p3);
  data→pacman := data→board→create_sprite_set;
  data→pacman→add($pac1);
  data→pacman→add($pac2);
  data→pacman→add($pac3);
  meta private;
}

var speed : Number {
}

action animateChomp() {
  data→frame := data→frame + data→mod;
  if data→frame = 0 then {
    data→pacman→at(0)→show;
//    //:: ExpectedOutput(assert.failed)
    data→pacman→at(1)→hide;
//    //:: ExpectedOutput(assert.failed)
    data→pacman→at(2)→hide;
    data→mod := 1;
  }
  if data→frame = 1 then {
    data→pacman→at(0)→hide;
//    //:: ExpectedOutput(assert.failed)
    data→pacman→at(1)→show;
//    //:: ExpectedOutput(assert.failed)
    data→pacman→at(2)→hide;
  }
  if data→frame = 2 then {
    data→pacman→at(0)→hide;
//    //:: ExpectedOutput(assert.failed)
    data→pacman→at(1)→hide;
//    //:: ExpectedOutput(assert.failed)
    data→pacman→at(2)→show;
    data→mod := - 1;
  }
  meta private;
}

action setPacDir(dir: Number) {
  $angle := 0;
  $vx := 0;
  $vy := 0;
  if $dir = 0 then {
    $vy := - data→speed;
    $angle := 270;
  }
  if $dir = 1 then {
    $vx := data→speed;
    $angle := 0;
  }
  if $dir = 2 then {
    $vy := data→speed;
    $angle := 90;
  }
  if $dir = 3 then {
    $vx := - data→speed;
    $angle := 180;
  }
  foreach sprite in data→pacman
  where true
  do {
    $sprite→set_speed($vx, $vy);
    $sprite→set_angle($angle);
  }
  meta private;
}

action setPacPos(x: Number, y: Number) {
  foreach sprite in data→pacman
  where true
  do {
    $sprite→set_pos($x, $y);
  }
  meta private;
}

var size : Number {
}

action makeGhost(c: Color) {
  $pic := media→create_picture(data→size, data→size);
  $pic→fill_ellipse(0, 0, data→size, data→size, 0, $c);
  $pic→fill_rect(0, data→size / 2, data→size, data→size, 0, $c);
  $pic→fill_rect(data→size / 4, data→size * .85, data→size / 4, data→size / 4, 45, colors→black);
  $pic→fill_rect(data→size * 3 / 4, data→size * .85, data→size / 4, data→size / 4, 45, colors→black);
  $pic→fill_rect(data→size / 2, data→size * .85, data→size / 4, data→size / 4, 45, colors→black);
  $pic→fill_ellipse(data→size / 3, data→size / 4, data→size / 4, data→size / 3, 0, colors→white);
  $pic→fill_ellipse(data→size * 2 / 3, data→size / 4, data→size / 4, data→size / 3, 0, colors→white);
  $pic→fill_rect((data→size / 3) + data→size / 16, (data→size / 4) + data→size / 8, data→size / 8, data→size / 8, 0, colors→blue→darken(0.1));
  $pic→fill_rect((data→size * 2 / 3) + data→size / 16, (data→size / 4) + data→size / 8, data→size / 8, data→size / 8, 0, colors→blue→darken(0.1));
  $sprite := data→board→create_picture($pic);
  data→ghosts→add($sprite);
  meta private;
}

var ghosts : Sprite_Set {
  transient = true;
}

action makePad() {
  $pic := media→create_picture(data→board→width, 260);
  $pic→fill_rect(0, 0, $pic→width, $pic→height, 0, colors→black);
  $midx := ($pic→width / 2) - 18;
  $midy := ($pic→height / 2) - 22;
  $hspace := 100;
  $vspace := 80;
  $pic→draw_text($midx, $midy - $vspace, "▲", 40, 0, colors→white);
  $pic→draw_text($midx + $hspace, $midy, "▶", 40, 0, colors→white);
  $pic→draw_text($midx, $midy + $vspace, "▼", 40, 0, colors→white);
  $pic→draw_text($midx - $hspace, $midy, "◀", 40, 0, colors→white);
  data→pad := data→board→create_picture($pic);
  data→pad→set_y(data→board→height - data→pad→height / 2);
  data→padv := math→create_vector3(data→pad→x, data→pad→y, 0);
  meta private;
}

var pad : Sprite {
  transient = true;
}

var padv : Vector3 {
  transient = true;
}

var dir : Number {
}

var run : Boolean {
}

var bw : Number {
}

var bh : Number {
}

var bg : Sprite {
  transient = true;
}
action makeGhosts() {
  code→makeGhost(colors→red);
  code→makeGhost(colors→red→lighten(0.7));
  code→makeGhost(colors→orange);
  code→makeGhost(colors→cyan→darken(0.1));
  data→ghosts→at(0)→set_pos(data→bw / 2, (data→bh / 2) - data→size * 3);
//  //:: ExpectedOutput(assert.failed)
//  data→ghosts→at(1)→set_pos(data→bw / 2, (data→bh / 2) - data→size);
//  //:: ExpectedOutput(assert.failed)
  data→ghosts→at(2)→set_pos((data→bw / 2) - data→size, (data→bh / 2) - data→size);
//  //:: ExpectedOutput(assert.failed)
  data→ghosts→at(3)→set_pos((data→bw / 2) + data→size, (data→bh / 2) - data→size);
  meta private;
}

var block : Number {
}
