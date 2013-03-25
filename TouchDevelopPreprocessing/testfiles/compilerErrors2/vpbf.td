meta version "v2.2,nothing";
meta name "game";
//
meta platform "current";
meta import playing_card_deck {
  pub "jvzu"
  usage {
    action shuffle()
    action deal() returns new_card: Json_Object
  }
  resolve suits = ♻suits resolve core = ♻core resolve array = ♻array with {
  }
  resolve collection = ♻collection resolve definition = ♻definition resolve color_helpers = ♻color_helpers resolve instance = ♻instance resolve card = ♻card with {
  }
}
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
  pub "lwhx"
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
  pub "cfjx"
  usage {
    action init()
    action rank(c: Json_Object) returns s: String
    action color(c: Json_Object) returns clr: Color
    action suit(c: Json_Object) returns s: String
  }
  resolve suits = ♻suits with {
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection resolve instance = ♻instance with {
  }
  resolve color_helpers = ♻instance resolve definition = ♻definition with {
  }
}

action main() {
  data→board := media→create_full_board;
  data→board→post_to_wall;
  data→board→set_background(colors→from_rgb(0, 0.4, 0));
  data→board→update_on_wall;
  data→busted := false;
  data→stand := false;
  ♻ card→init;
  ♻ definition→init;
  ♻ playing_card_deck→shuffle;
  data→total := 0;
  data→compTotal := 0;
  data→totalsprite := code→newText("Total: " ∥ data→total, colors→white, 40, 40, 250, 300);
  data→totalsprite→set_pos(250, 250);
  data→cardPos := 20;
  data→hotspots := data→board→create_sprite_set;
  code→newButton("Hit", 70, 350);
  code→newButton("Stand", 200, 350);
  code→newButton("New", 350, 350);
  data→compCardPos := 20;
  code→playCard;
  code→playCard;
  code→playCardComp;
  data→board→update_on_wall;
}
var card1 : Json_Object {
}
var card2 : Json_Object {
}
var hotspots : Sprite_Set {
}

action newPicture(pic: Picture, left: Number, @top: Number) returns sprite: Sprite {
  $sprite := data→board→create_picture($pic);
  $sprite→set_pos($left + $pic→width / 2, $@top + $pic→height / 2);
}
var board : Board {
}

action newText(text: String, color: Color, size: Number, width: Number, left: Number, @top: Number) returns sprite1: Sprite {
  $height := 1.5 * $size;
  $sprite1 := data→board→create_text($width, $height, $size, $text);
  $sprite1→set_pos($left + $width / 2, $height / 2 + $@top);
  $sprite1→set_color($color);
}

action showCard(card1: Json_Object, left: Number, @top: Number) {
  $rectsprite := data→board→create_rectangle(150, 200);
  $rectsprite→set_color(colors→white);
  $rectsprite→set_pos($left + 75, $@top + 100);
  $rectsprite;
  data→hotspots→add($rectsprite);
  $cardsuit := code→newText(♻ card→suit($card1), ♻ card→color($card1), 40, 40, $left + 10, $@top + 50);
  $cardsuitmid := code→newText(♻ card→suit($card1), ♻ card→color($card1), 60, 60, $left + 50, $@top + 65);
  $cardsuitflip := code→newText(♻ card→suit($card1), ♻ card→color($card1), 40, 40, $left + 110, $@top + 110);
  $cardrank := code→newText(♻ card→rank($card1), ♻ card→color($card1), 40, 40, $left + 10, $@top + 10);
  $cardrankflip := code→newText(♻ card→rank($card1), ♻ card→color($card1), 40, 40, $left + 110, $@top + 150);
  $cardrankflip→set_angle(180);
  $cardsuitflip→set_angle(180);
  data→hotspots→add($cardsuit);
  data→hotspots→add($cardrank);
  data→hotspots→add($cardrankflip);
  data→hotspots→add($cardsuitflip);
  data→hotspots→add($cardsuitmid);
}

action newButton(name: String, left: Number, @top: Number) {
  $button := data→board→create_text(50, 50, 40, $name);
  $button→set_pos($left, $@top);
  data→hotspots→add($button);
}

event tap_sprite_in_hotspots(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  if $index_in_set = 0 and not data→busted and not data→stand then {
    code→playCard;
  }
  else {
    if $index_in_set = 2 then {
      "Coming here " →post_to_wall;
      code→main;
    }
    else {
      if $index_in_set = 1 then {
        data→stand := true;
        while data→compTotal ≤ 21 and data→compTotal ≤ data→total do {
          code→playCardComp;
        }
        if data→compTotal > 21 then {
          code→gameWin;
        }
        else {
          code→gameLose;
        }
      }
    }
  }
  data→board→update_on_wall;
  meta private;
}

action playCard() {
  $card1 := ♻ playing_card_deck→deal();
  code→showCard($card1, data→cardPos, 20);
  $x := ♻ card→rank($card1);
  data→total := data→total + code→getCardValue($x);
  data→cardPos := data→cardPos + 60;
  data→totalsprite→set_text("Total: " ∥ data→total);
  if data→total > 21 then {
    code→gameOver;
  }
  data→board→update_on_wall;
}
var cardPos : Number {
}
var total : Number {
}
var totalsprite : Sprite {
}

action getCardValue(rank: String) returns value: Number {
  if($rank→equals("A") or $rank→equals("K") or $rank→equals("Q") or $rank→equals("J")) then {
    $value := 10;
  }
  else {
    $value := $rank→to_number;
  }
}

action gameOver() {
  $over := data→board→create_text(100, 20, 60, "Bust!!");
  $over→set_pos(200, 450);
  data→busted := true;
  data→board→update_on_wall;
}
var busted : Boolean {
}

action playCardComp() {
  $card1 := ♻ playing_card_deck→deal();
  code→showCard($card1, data→compCardPos, 540);
  $x := ♻ card→rank($card1);
  data→compTotal := data→compTotal + code→getCardValue($x);
  data→compCardPos := data→compCardPos + 60;
  data→board→update_on_wall;
}
var compCardPos : Number {
}
var compTotal : Number {
}

action gameWin() {
  $win := data→board→create_text(100, 20, 80, "You Win :-)");
  $win→set_pos(100, 400);
  $win→set_opacity(100);
  data→board→update_on_wall;
}

action gameLose() {
  $loss := data→board→create_text(100, 20, 80, "You Lose :(");
  $loss→set_pos(100, 400);
  $loss→set_opacity(100);
  data→board→update_on_wall;
}
var stand : Boolean {
}

