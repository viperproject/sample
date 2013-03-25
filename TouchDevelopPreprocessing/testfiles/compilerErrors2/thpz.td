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
  data→board→set_background(colors→from_rgb(0, 0.4, 0));
  data→board→post_to_wall;
  ♻ card→init;
  ♻ definition→init;
  ♻ playing_card_deck→shuffle;
  data→card1 := ♻ playing_card_deck→deal();
  data→hotspots := data→board→create_sprite_set;
  data→card2 := ♻ playing_card_deck→deal();
  code→showCard(data→card1, 20, 20);
  code→showCard(data→card2, 300, 20);
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

