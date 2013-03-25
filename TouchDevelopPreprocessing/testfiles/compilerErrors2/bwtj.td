meta version "v2.2,nothing";
meta name "High Low";
meta icon "files";
meta color "#ff007fff";
// Simple card game created for testing my libraries.
//
// Check out all my games and utilities by visiting my user profile page on touchdevelop.com or tapping my user name on the details page of any of my scripts in the TouchDevelop app.
meta platform "current";
meta import deck {
  pub "iush"
  usage {
    action shuffle()
    action deal() returns new_card: Json_Object
  }
  resolve card = ♻card with {
  }
  resolve suits = ♻suits resolve core = ♻core resolve array = ♻array with {
  }
  resolve collection = ♻collection resolve definition = ♻definition resolve color_helpers = ♻color_helpers resolve instance = ♻instance}
meta import suits {
  pub "qzig"
  usage {
  }
}
meta import core {
  pub "dnoo"
  usage {
  }
}
meta import array {
  pub "bzjc"
  usage {
  }
  resolve core = ♻core with {
  }
}
meta import collection {
  pub "tizp"
  usage {
  }
  resolve core = ♻core with {
  }
}
meta import definition {
  pub "txih"
  usage {
    action init()
  }
  resolve core = ♻core resolve array = ♻array with {
  }
  resolve collection = ♻collection with {
  }
}
meta import color_helpers {
  pub "wszv"
  usage {
  }
}
meta import instance {
  pub "sbog"
  usage {
  }
  resolve core = ♻core with {
  }
  resolve array = ♻array resolve color_helpers = ♻color_helpers with {
  }
  resolve collection = ♻collection with {
  }
}
meta import card {
  pub "bihw"
  usage {
    action init()
    action rank(c: Json_Object) returns s: String
    action color(c: Json_Object) returns clr: Color
    action suit(c: Json_Object) returns s: String
  }
  resolve suits = ♻suits with {
  }
  resolve definition = ♻definition with {
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection resolve instance = ♻instance with {
  }
  resolve color_helpers = ♻color_helpers}

action main() {
  code→gameInit;
  code→gameLoad;
  code→gameDraw;
}

action gameInit() {
  ♻ definition→init;
  ♻ card→init;
  ♻ deck→shuffle;
  code→deal_cards;
  data→landscape := false;
  data→betting := true;
  meta private;
}

action gameLoad() {
  $reload := false;
  if data→button→is_invalid or $reload then {
    data→button := media→create_picture(48, 48);
    data→button→draw_ellipse(0, 0, 48, 48, 0, colors→foreground, 3);
  }
  if data→lower→is_invalid or $reload then {
    data→lower := media→icon("thumbsdown");
    if colors→is_light_theme then {
      data→lower→colorize(colors→transparent, colors→foreground, 0.65);
    }
  }
  if data→higher→is_invalid or $reload then {
    data→higher := media→icon("thumbsdown")→clone;
    $w := data→higher→width;
    $h := data→higher→height;
    for 0 ≤ r < data→higher→height / 2 do {
      for 0 ≤ c < data→higher→width do {
        $swap := data→higher→at($r * $w + $c);
        $x := $h - $r - 1;
        data→higher→set_pixel($c, $r, data→higher→at($x * $w + $c));
        data→higher→set_pixel($c, $x, $swap);
      }
    }
    if colors→is_light_theme then {
      data→lower→colorize(colors→transparent, colors→foreground, 0.65);
    }
  }
  if data→card_back→is_invalid or $reload then {
    data→card_back := media→create_picture(180, 270);
    $radius := 15;
    $diameter := $radius * 2;
    $c1 := colors→black;
    for 0 ≤ i2 < 9 do {
      for 0 ≤ i1 < 6 do {
        if $c1→equals(colors→black) then {
          $c1 := colors→red;
        }
        else {
          $c1 := colors→black;
        }
        data→card_back→fill_ellipse($i1 * $diameter, $i2 * $diameter, $radius, $radius, 0, $c1);
        if $c1→equals(colors→black) then {
          $c1 := colors→red;
        }
        else {
          $c1 := colors→black;
        }
        data→card_back→fill_ellipse($i1 * $diameter + $radius, $i2 * $diameter + $radius, $radius, $radius, 0, $c1);
      }
    }
  }
  if data→blank_card→is_invalid or $reload then {
    data→blank_card := media→create_picture(198, 288);
    $radius1 := 8;
    $diameter1 := $radius1 * 2;
    data→blank_card→fill_ellipse(0, 0, $diameter1, $diameter1, 0, colors→white);
    data→blank_card→fill_ellipse(data→blank_card→width - $diameter1, 0, $diameter1, $diameter1, 0, colors→white);
    data→blank_card→fill_ellipse(data→blank_card→width - $diameter1, data→blank_card→height - $diameter1, $diameter1, $diameter1, 0, colors→white);
    data→blank_card→fill_ellipse(0, data→blank_card→height - $diameter1, $diameter1, $diameter1, 0, colors→white);
    data→blank_card→fill_rect($radius1, 0, data→blank_card→width - $diameter1, data→blank_card→height, 0, colors→white);
    data→blank_card→fill_rect(0, $radius1, $diameter1, data→blank_card→height - $diameter1, 0, colors→white);
    data→blank_card→fill_rect(data→blank_card→width - $diameter1, $radius1, $diameter1, data→blank_card→height - $diameter1, 0, colors→white);
  }
  meta private;
}

action gameDraw() {
  data→board := media→create_full_board;
  data→board→set_background(colors→from_rgb(0, 0.4, 0));
  data→board→post_to_wall;
  data→hotspots := data→board→create_sprite_set;
  code→newText("HIGH LOW", colors→yellow, 90, 480, 0, 0);
  data→display := code→newText("Points:  " ∥ data→points, colors→yellow, 30, 480, 0, 550);
  $show := code→newPicture(data→blank_card, 30, 200);
  $bet := code→newPicture(data→blank_card, 252, 200);
  data→bet_back := code→newPicture(data→card_back, 261, 209);
  data→show_rank := code→newText(♻ card→rank(data→show_card), ♻ card→color(data→show_card), 40, 40, 40, 200);
  data→bet_rank := code→newText(♻ card→rank(data→bet_card), ♻ card→color(data→bet_card), 40, 40, 262, 200);
  data→bet_rank→hide;
  data→show_rank_flip := code→newText(data→show_rank→text, data→show_rank→color, 40, 40, 178, 428);
  data→bet_rank_flip := code→newText(data→bet_rank→text, data→bet_rank→color, 40, 40, 400, 428);
  data→show_rank_flip→set_angle(180);
  data→bet_rank_flip→set_angle(180);
  data→bet_rank_flip→hide;
  data→show_suit := code→newText(♻ card→suit(data→show_card), data→show_rank→color, 100, 100, 40, 210);
  data→bet_suit := code→newText(♻ card→suit(data→bet_card), data→bet_rank→color, 100, 100, 40, 210);
  data→show_suit→move_towards($show, 1);
  data→bet_suit→move_towards($bet, 1);
  data→bet_suit→hide;
  code→newRectangle(colors→chrome, 480, 72, 0, 656);
  code→newButton(data→lower, 174);
  code→newButton(data→higher, 258);
  code→newHotspot($show);
  code→newHotspot($bet);
  data→board→update_on_wall;
  meta private;
}
var board : Board {
}
var hotspots : Sprite_Set {
}

event tap_sprite_in_hotspots(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $index_in_set = 0 then {
    if data→betting then {
      code→score(false);
    }
  }
  else {
    if $index_in_set = 1 then {
      if data→betting then {
        code→score(true);
      }
    }
    else {
      if not data→betting then {
        code→deal;
      }
    }
  }
  data→board→update_on_wall;
  meta private;
}

action newText(text: String, color: Color, size: Number, width: Number, left: Number, @top: Number) returns sprite: Sprite {
  $height := 1.5 * $size;
  $sprite := data→board→create_text($width, $height, $size, $text);
  $sprite→set_color($color);
  if data→landscape then {
    $sprite→set_angle(270);
    $sprite→set_pos($@top + $height / 2, data→board→height - ($left + $width / 2));
  }
  else {
    $sprite→set_pos($left + $width / 2, $@top + $height / 2);
  }
  meta private;
}
var landscape : Boolean {
}

action newRectangle(color: Color, width: Number, height: Number, left: Number, @top: Number) returns sprite: Sprite {
  $sprite := data→board→create_rectangle($width, $height);
  $sprite→set_color($color);
  if data→landscape then {
    $sprite→set_angle(270);
    $sprite→set_pos($@top + $height / 2, data→board→height - ($left + $width / 2));
  }
  else {
    $sprite→set_pos($left + $width / 2, $@top + $height / 2);
  }
  meta private;
}

action newPicture(pic: Picture, left: Number, @top: Number) returns sprite: Sprite {
  $sprite := data→board→create_picture($pic);
  if data→landscape then {
    $sprite→set_angle(270);
    $sprite→set_pos($@top + $pic→height / 2, data→board→height - ($left + $pic→width / 2));
  }
  else {
    $sprite→set_pos($left + $pic→width / 2, $@top + $pic→height / 2);
  }
  meta private;
}
var button : Picture {
}

action newButton(image: Picture, pos: Number) {
  $button := code→newPicture(data→button, $pos, 668);
  $pic := code→newPicture($image, 0, 0);
  code→newHotspot($button);
  $pic→move_towards($button, 1);
  meta private;
}
var lower : Picture {
}
var higher : Picture {
}

action newHotspot(over: Sprite) returns sprite: Sprite {
  $sprite := data→board→create_rectangle($over→width, $over→height);
  $sprite→set_color(colors→transparent);
  $sprite→set_angle($over→angle);
  $sprite→set_pos($over→x, $over→y);
  data→hotspots→add($sprite);
  meta private;
}
var blank_card : Picture {
}
var card_back : Picture {
}
var show_card : Json_Object {
}
var bet_card : Json_Object {
}
var points : Number {
}

action deal_cards() {
  data→show_card := ♻ deck→deal();
  data→bet_card := ♻ deck→deal();
  meta private;
}
var show_rank : Sprite {
}
var show_suit : Sprite {
}
var bet_back : Sprite {
}
var show_rank_flip : Sprite {
}
var bet_rank : Sprite {
}
var bet_rank_flip : Sprite {
}
var bet_suit : Sprite {
}
var betting : Boolean {
}

action deal() {
  code→deal_cards;
  data→betting := true;
  data→bet_rank→hide;
  data→bet_rank_flip→hide;
  data→bet_suit→hide;
  data→bet_back→show;
  data→board→update_on_wall;
  data→bet_rank→set_text(♻ card→rank(data→bet_card));
  data→bet_rank→set_color(♻ card→color(data→bet_card));
  data→bet_rank_flip→set_text(data→bet_rank→text);
  data→bet_rank_flip→set_color(data→bet_rank→color);
  data→bet_suit→set_text(♻ card→suit(data→bet_card));
  data→bet_suit→set_color(data→bet_rank→color);
  data→show_rank→set_text(♻ card→rank(data→show_card));
  data→show_rank→set_color(♻ card→color(data→show_card));
  data→show_rank_flip→set_text(data→show_rank→text);
  data→show_rank_flip→set_color(data→show_rank→color);
  data→show_suit→set_text(♻ card→suit(data→show_card));
  data→show_suit→set_color(data→show_rank→color);
  meta private;
}

action score(up: Boolean) {
  $point := 0;
  $show := ♻ card→rank(data→show_card);
  $bet := ♻ card→rank(data→bet_card);
  if not $show→equals($bet) then {
    if $bet→equals("A") then {
      $point := 1;
    }
    else {
      $down := false;
      if $bet→equals("K") then {
        $down := $show→equals("A");
      }
      else {
        if $bet→equals("Q") then {
          $down := "AK" →contains($show);
        }
        else {
          if $bet→equals("J") then {
            $down := "AKQ" →contains($show);
          }
          else {
            if "AKQJ" →contains($show) then {
              $down := true;
            }
            else {
              $down := $bet→to_number < $show→to_number;
            }
          }
        }
      }
      if($up or $down) and not $up→equals($down) then {
        $point := 1;
      }
      else {
        $point := - 1;
      }
    }
  }
  data→points := data→points + $point;
  bazaar→post_leaderboard_score(data→points);
  data→display→set_text("Points:  " ∥ data→points);
  data→betting := false;
  data→bet_rank→show;
  data→bet_rank_flip→show;
  data→bet_suit→show;
  data→bet_back→hide;
  meta private;
}
var display : Sprite {
}

