meta version "v2.2,nothing";
meta name "testSprites";
//
meta platform "current";
meta import spritesFromPic {
  pub "pfdkxvqd"
  usage {
    action spritesFromPic(pic: Picture, picX: Number, picY: Number, board: Board, boardX: Number, boardY: Number, radius: Number, grid: Number, onlyUseCircle: Boolean, circularSprites: Boolean, spriteSize: Number) returns createdSprites: Sprite_Set
  }
}
meta import getPhotoBoard {
  pub "njpjogji"
  usage {
    action getPhotoBoard() returns pic: Picture, board: Board
  }
}

action main() {
  code→init;
  data→pic, data→board := ♻ getPhotoBoard→getPhotoBoard;
  $sprite := data→board→create_picture(data→pic);
  $sprite→set_pos(data→board→width / 2, data→board→height / 2);
  data→debug := data→board→create_text(100, 20, 40, "debug");
  data→debug→hide;
  data→board→post_to_wall;
  skip;
}

event gameloop() {
  data→board→evolve;
  data→debug→set_text(data→sprites→at(10)→speed_x→to_string ∥ "  " ∥ data→sprites→count→to_string);
  data→board→update_on_wall;
  meta private;
}
var pic : Picture {
}
var board : Board {
}

action init() {
  data→speed := 50;
  meta private;
}
var speed : Number {
}

event tap_board\u003A_board(x: Number, y: Number) {
  data→sprites := data→board→create_sprite_set;
  data→sprites := ♻ spritesFromPic→spritesFromPic(data→pic, $x, $y, data→board, $x, $y, 50, 10, true, false, 10);
  foreach e in data→sprites where true do {
    $e→set_speed(math→random(data→speed) - data→speed / 2, math→random(data→speed) - data→speed / 2);
  }
  meta private;
}
var sprites : Sprite_Set {
}
var debug : Sprite {
}

