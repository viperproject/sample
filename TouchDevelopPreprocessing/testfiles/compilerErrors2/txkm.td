meta version "v2.2,nothing";
meta name "data pic";
meta icon "document";
meta color "#ff007fff";
// Saving and loading string to and from saved pictures album..
meta isLibrary "yes";
meta platform "current";
meta import base_string {
  pub "lmzr"
  usage {
    action btd(base_str: String, base: Number, length: Number) returns decimal: Number
    action dtb(decimal: Number, base: Number, length: Number) returns base_str: String
  }
}

action main() {
  $s := "A longer test with rows and special chars:\n(&%$#@)<{[^]}>€£¥*-+=_~|√¦";
  $s→post_to_wall;
  code→save_string($s, "");
  code→load_string→post_to_wall;
}

action color_to_base_3(color: Color) returns string: String {
  $string := "";
  $max := 0;
  $max_indx := 0;
  $zero := collections→create_number_collection;
  $one := collections→create_number_collection;
  $color_comp := collections→create_number_collection;
  $color_comp→add($color→B);
  $color_comp→add($color→G);
  $color_comp→add($color→R);
  for 0 ≤ i < 3 do {
    if $color_comp→at($i) = 0 then {
      $zero→add($i);
    }
    if $color_comp→at($i) = 1 then {
      $one→add($i);
    }
    if $color_comp→at($i) > $max then {
      $max := $color_comp→at($i);
      $max_indx := $i;
    }
  }
  if $one→count = 1 and $zero→count = 2 then {
    $string := $one→at(0)→to_string;
  }
  if $one→count > 1 or $zero→count > 2 then {
    $string := 3→to_string;
  }
  if $string→is_empty then {
    $string := $max_indx→to_string;
  }
  meta private;
}

action base_3_to_color(string: String) returns color: Color {
  $v := collections→create_number_collection;
  $x := $string→to_number;
  for 0 ≤ i < 3 do {
    if $i = $x then {
      $v→add(1);
    }
    else {
      $v→add(0);
    }
  }
  $color := colors→from_argb(1, $v→at(2), $v→at(1), $v→at(0));
  meta private;
}

action pic_to_string(pic: Picture) returns string: String {
  $string := "";
  $char_base_3 := "";
  $char_block := 0;
  $@continue := true;
  $pic_index := 0;
  $pic_resized := $pic→clone;
  $pic_resized→resize($pic→width / 10, math→ceiling($pic→height / 10));
  $j := 0;
  $k := 0;
  while $@continue and $pic_index < $pic→count do {
    $char_base_3 := $char_base_3 ∥ code→color_to_base_3($pic_resized→pixel($j, $k));
    $j := $j + 1;
    if $j ≥ $pic_resized→width then {
      $j := 0;
      $k := $k + 1;
      if $k ≥ $pic_resized→height then {
        $@continue := false;
      }
    }
    $pic_index := $pic_index + 1;
    $char_block := $char_block + 1;
    if $char_block ≥ 10 then {
      $char_number := ♻ base_string→btd($char_base_3, 3, 10);
      $string := $string ∥ $char_number→to_character;
      $char_base_3 := "";
      $char_block := 0;
      if $string→count > 6 then {
        $EOF := $string→substring($string→count - 3, 3);
        if $EOF→equals("EOF") then {
          $string := $string→substring(0, $string→count - 3);
          $@continue := false;
        }
      }
    }
  }
  meta private;
}

action string_to_pic(string: String) returns pic: Picture {
  $str_base_3 := code→string_to_base_3($string);
  $pic_width := 48;
  $pic_hight := math→max(math→ceiling($str_base_3→count / $pic_width), 10);
  $pic_hight := math→ceiling($str_base_3→count / $pic_width);
  $pic := media→create_picture($pic_width, $pic_hight);
  $j := 0;
  $k := 0;
  for 0 ≤ i < $str_base_3→count do {
    $pic→set_pixel($j, $k, code→base_3_to_color($str_base_3→at($i)));
    $j := $j + 1;
    if $j ≥ $pic→width then {
      $j := 0;
      $k := $k + 1;
    }
  }
  $pic→resize($pic_width * 10, $pic_hight * 10);
  meta private;
}

action string_to_base_3(string: String) returns str_base_3: String {
  $str_base_3 := "";
  foreach s in $string where true do {
    $char_unicode := $s→to_unicode;
    $str_base_3 := $str_base_3 ∥ ♻ base_string→dtb($char_unicode, 3, 10);
  }
  $str_base_3 := $str_base_3 ∥ ♻ base_string→dtb("E" →to_unicode, 3, 10);
  $str_base_3 := $str_base_3 ∥ ♻ base_string→dtb("O" →to_unicode, 3, 10);
  $str_base_3 := $str_base_3 ∥ ♻ base_string→dtb("F" →to_unicode, 3, 10);
  meta private;
}

action save_string(string: String, title: String) {
  if not $string→is_empty then {
    if $title→is_empty then {
      $title := $string→substring(0, math→min($string→count, 100)) ∥ "...";
    }
    $pic_data := code→string_to_pic($string);
    $pic := media→create_picture($pic_data→width, $pic_data→height + 400);
    $pic→blend($pic_data, 0, 0, 0, 1);
    $line_y := 10 + $pic_data→height;
    $strDateTime := time→now→to_string→replace_regex("(?<yyyy>\\d{4})-(?<mm>\\d{2})-(?<dd>\\d{2}) (?<hr>\\d{2}:\\d{2}:\\d{2})", "${dd}.${mm}.${yyyy} ${hr}")→replace("Z", "");
    $pic→draw_text(0, $line_y, $strDateTime, 16, 0, colors→foreground);
    $line_y := $line_y + 30;
    $pic→draw_text(0, $line_y, $title, 16, 0, colors→foreground);
    $pic→save_to_library;
  }
}

action load_string() returns string: String {
  $string := "";
  $pic := invalid→picture;
  $pic := media→choose_picture;
  if not $pic→is_invalid then {
    $string := code→pic_to_string($pic);
  }
}

