meta version "v2.2,nothing";
meta name "mirror Camera";
meta icon "camera";
meta color "#ff00008b";
//
meta platform "current";

action main() {
  wall→set_title("Mirror Cam");
  wall→set_background(colors→from_argb(0.8, 0.1, 0.1, 0.3));
  $menu := "1.) left vertical,2.) right vertical,,3.) upper horizontal,4.) lower horizontal,,5.) A vertical and horizontal,6.) B vertical and horizontal" →split(",");
  $x := wall→pick_string("", "select mirror mode", $menu);
  $pic := senses→take_camera_picture;
  if $x = 0 then {
    $pic := code→pic_mirror($pic, true, true, true);
    // ##mirror vertically, use left half picture
    // ##mirror left and right from the middle line ( X ).
  }
  else {
    if $x = 1 then {
      $pic := code→pic_mirror($pic, true, false, true);
      // ##mirror vertically, use right half picture
      // ##mirror left and right from the middle line ( X ).
    }
    else {
      if $x = 3 then {
        $pic := code→pic_mirror($pic, false, true, true);
        // ##mirror horizontally, use upper half picture
        // ##mirror left and right from the middle line ( X ).
      }
      else {
        if $x = 4 then {
          $pic := code→pic_mirror($pic, false, false, true);
          // ##mirror horizontally, use lower half of picture
          // ##mirror left and right from the middle line ( X ).
        }
        else {
          if $x = 6 then {
            $pic := code→mirror_horizontally\u0026vertically($pic, true, true, true);
          }
          else {
            if $x = 7 then {
              $pic := code→mirror_horizontally\u0026vertically($pic, true, false, false);
            }
          }
        }
      }
    }
  }
  wall→clear;
  $pic→post_to_wall;
  $save_pic := wall→ask_boolean("save pic to library?", "");
  wall→clear;
  if $save_pic then {
    $s := $pic→save_to_library;
    $pic→post_to_wall;
    $s→post_to_wall;
  }
  $@continue := wall→ask_boolean("continue?", "");
  if $@continue then {
    wall→clear;
    $pic := invalid→picture;
    code→main;
  }
  else {
    time→stop_and_close;
    wall→clear;
  }
}

action pic_left_A_mirror_vertical(pic: Picture) returns mirrored_pic: Picture {
  $h, $w, $mirrored_pic, $pic1 := code→sub_mirror($pic);
  $pic1→crop(0, $h / 2, $w, $h / 2);
  $pic2 := $pic1→clone;
  $pic1→flip_vertical;
  $mirrored_pic→blend($pic2, 0, 0, 0, 1);
  $mirrored_pic→blend($pic1, 0, $h / 2, 0, 1);
  meta private;
}

action pic_upper_A_mirror_horizontal(pic: Picture) returns mirrored_pic: Picture {
  $h, $w, $mirrored_pic, $pic1 := code→sub_mirror($pic);
  $pic1→crop(0, 0, $w / 2, $h);
  $pic2 := $pic1→clone;
  $pic2→flip_horizontal;
  $mirrored_pic→blend($pic1, 0, 0, 0, 1);
  $mirrored_pic→blend($pic2, $w / 2, 0, 0, 1);
  meta private;
}

action pic_mirr_A_horizontal\u0026vertical(pic: Picture) returns pic2: Picture {
  $pic1 := code→pic_left_A_mirror_vertical($pic);
  $pic2 := code→pic_upper_A_mirror_horizontal($pic1);
  meta private;
}

action pic_left_B_mirror_vertical(pic: Picture) returns mirrored_pic: Picture {
  $h, $w, $mirrored_pic, $pic1 := code→sub_mirror($pic);
  $pic1→crop(0, $h / 2, $w, $h / 2);
  $pic2 := $pic1→clone;
  $pic1→flip_vertical;
  $mirrored_pic→blend($pic1, 0, 0, 0, 1);
  $mirrored_pic→blend($pic2, 0, $h / 2, 0, 1);
  meta private;
}

action pic_upper_B_mirr__horizontal(pic: Picture) returns mirrored_pic: Picture {
  $h, $w, $mirrored_pic, $pic1 := code→sub_mirror($pic);
  $pic1→crop(0, 0, $w / 2, $h);
  $pic2 := $pic1→clone;
  $pic1→flip_horizontal;
  $mirrored_pic→blend($pic2, 0, 0, 0, 1);
  $mirrored_pic→blend($pic1, $w / 2, 0, 0, 1);
  meta private;
}

action pic_mirr_B_horizontal\u0026vertical(pic: Picture) returns pic2: Picture {
  $pic1 := code→pic_right_A_mirror_vertical($pic);
  $pic2 := code→pic_lower_A_mirror_horizontal($pic1);
  meta private;
}

action pic_right_A_mirror_vertical(pic: Picture) returns mirrored_pic: Picture {
  $h, $w, $mirrored_pic, $pic1 := code→sub_mirror($pic);
  $pic1→crop(0, 0, $w, $h / 2);
  $pic2 := $pic1→clone;
  $pic1→flip_vertical;
  $mirrored_pic→blend($pic2, 0, 0, 0, 1);
  $mirrored_pic→blend($pic1, 0, $h / 2, 0, 1);
  meta private;
}

action pic_lower_A_mirror_horizontal(pic: Picture) returns mirrored_pic: Picture {
  $h, $w, $mirrored_pic, $pic1 := code→sub_mirror($pic);
  $pic1→crop($w / 2, 0, $w / 2, $h);
  $pic2 := $pic1→clone;
  $pic2→flip_horizontal;
  $mirrored_pic→blend($pic1, 0, 0, 0, 1);
  $mirrored_pic→blend($pic2, $w / 2, 0, 0, 1);
  meta private;
}

action pic_right_B_mirror_vertical(pic: Picture) returns mirrored_pic: Picture {
  $h, $w, $mirrored_pic, $pic1 := code→sub_mirror($pic);
  $pic1→crop(0, 0, $w, $h / 2);
  $pic2 := $pic1→clone;
  $pic1→flip_vertical;
  $mirrored_pic→blend($pic1, 0, 0, 0, 1);
  $mirrored_pic→blend($pic2, 0, $h / 2, 0, 1);
  meta private;
}

action pic_lower_B_mirror_horizontal(pic: Picture) returns mirrored_pic: Picture {
  $h, $w, $mirrored_pic, $pic1 := code→sub_mirror($pic);
  $pic1→crop($w / 2, 0, $w / 2, $h);
  $pic2 := $pic1→clone;
  $pic2→flip_horizontal;
  $mirrored_pic→blend($pic2, 0, 0, 0, 1);
  $mirrored_pic→blend($pic1, $w / 2, 0, 0, 1);
  meta private;
}

action pic_mirr_AB__horizontal\u0026vertical(pic: Picture) returns pic2: Picture {
  $pic1 := code→pic_left_B_mirror_vertical($pic);
  $pic2 := code→pic_upper_B_mirr__horizontal($pic1);
}

action pic_mirr_BB_horizontal\u0026vertical(pic: Picture) returns pic2: Picture {
  $pic1 := code→pic_right_B_mirror_vertical($pic);
  $pic2 := code→pic_lower_B_mirror_horizontal($pic1);
}

action pic_mirror(pic: Picture, vertical: Boolean, left_or_upper: Boolean, X: Boolean) returns mirrored_pic: Picture {
  // pic: picture,  vertical: boolean, upper or left: boolean, mirror mode X: boolean
  // (middle, outside)
  $h := $pic→height;
  $w := $pic→width;
  $mirrored_pic := media→create_picture($w, $h);
  $pic1 := $pic→clone;
  // ##crop pic
  if $vertical and $left_or_upper then {
    $pic1→crop(0, $h / 2, $w, $h / 2);
  }
  else {
    if $vertical and not $left_or_upper then {
      $pic1→crop(0, 0, $w, $h / 2);
    }
    else {
      if not $vertical and $left_or_upper then {
        $pic1→crop(0, 0, $w / 2, $h);
      }
      else {
        if not ($vertical and $left_or_upper) then {
          $pic1→crop($w / 2, 0, $w / 2, $h);
        }
      }
    }
  }
  //
  $pic2 := $pic1→clone;
  if $vertical then {
    $pic1→flip_vertical;
  }
  else {
    $pic1→flip_horizontal;
  }
  //
  if $vertical and $X then {
    $mirrored_pic→blend($pic2, 0, 0, 0, 1);
    $mirrored_pic→blend($pic1, 0, $h / 2, 0, 1);
  }
  else {
    if $vertical and not $X then {
      $mirrored_pic→blend($pic1, 0, 0, 0, 1);
      $mirrored_pic→blend($pic2, 0, $h / 2, 0, 1);
    }
    else {
      //
    }
  }
  //
  if not $vertical and $X then {
    $mirrored_pic→blend($pic1, 0, 0, 0, 1);
    $mirrored_pic→blend($pic2, $w / 2, 0, 0, 1);
  }
  else {
    if not $vertical and not $X then {
      $mirrored_pic→blend($pic2, 0, 0, 0, 1);
      $mirrored_pic→blend($pic1, $w / 2, 0, 0, 1);
    }
  }
  meta private;
}

action sub_mirror(pic: Picture) returns h: Number, w: Number, mirrored_pic: Picture, pic1: Picture {
  $h := $pic→height;
  $w := $pic→width;
  $mirrored_pic := media→create_picture($w, $h);
  $pic1 := $pic→clone;
  meta private;
}

action go2() {
  wall→set_reversed(true);
  $pic := senses→take_camera_picture;
  $pic→resize(240, - 1);
  for 0 ≤ i < 2 do {
    for 0 ≤ i1 < 2 do {
      for 0 ≤ i2 < 2 do {
        $b1 := $i1 = 1;
        $b2 := $i2 = 1;
        $b := $i = 1;
        $pic1 := code→pic_mirror($pic, $b, $b1, $b2);
        $pic1→post_to_wall;
        ($b→to_string ∥ " " ∥ $b1→to_string ∥ " " ∥ $b2→to_string)→post_to_wall;
      }
    }
  }
  for 0 ≤ i3 < 2 do {
    for 0 ≤ i4 < 2 do {
      for 0 ≤ i5 < 2 do {
        $b3 := $i3 = 1;
        $b4 := $i4 = 1;
        $b5 := $i5 = 1;
        $pic2 := code→mirror_horizontally\u0026vertically($pic, $b3, $b4, $b5);
        $pic2→post_to_wall;
      }
    }
  }
}

action mirror_horizontally\u0026vertically(pic: Picture, left: Boolean, upper: Boolean, X: Boolean) returns pic2: Picture {
  $pic1 := code→pic_mirror($pic, true, $left, $X);
  $pic2 := code→pic_mirror($pic1, false, $upper, $X);
  // ##mirror horizontally, use upper half picture
  // ##mirror left and right from the middle line ( X ).
  // ##mirror vertically, use left half picture
  // ##mirror left and right from the middle line ( X ).
}

