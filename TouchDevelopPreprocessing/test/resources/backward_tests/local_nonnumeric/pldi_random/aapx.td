meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "image tiler";
meta icon "cut";
meta rootId "lXuaQNuC6YrSns2MBQrEqXho";
meta platform "current";
// Cuts an image in 4 tiles. pin the actions and see your pictures on your start screen!

//:: ExpectedOutput(backward.counterexample.found)
action main() {
  "Don\'t Forget to pin the actions!" →post_to_wall;
  $pic := media→choose_picture;
  $c := 173;
  $m := 12;
  //:: ExpectedOutput(assert.failed)
  $pic→resize(2 * $c + $m, - 1);
  code→crop_icon($pic, $c, 0, 0, data→top_left_tile);
  code→crop_icon($pic, $c, $c + $m, 0, data→top_right_tile);
  code→crop_icon($pic, $c, 0, $c + $m, data→lower_left_tile);
  code→crop_icon($pic, $c, $c + $m, $c + $m, data→lower_right_tile);

  meta private;
}

action top_left() {
  code→main;
}

action top_right() {
  code→main;
}

action lower_left() {
  code→main;
}

action lower_right() {
  code→main;
}

var lower_left_tile : Tile {
  readonly = true;
  transient = true;
}

var lower_right_tile : Tile {
  readonly = true;
  transient = true;
}

var top_left_tile : Tile {
  readonly = true;
  transient = true;
}

var top_right_tile : Tile {
  readonly = true;
  transient = true;
}

action crop_icon(pic: Picture, c: Number, left: Number, right: Number, tile: Tile) {
  $tile_image := $pic→clone;
  $tile_image→crop($left, $right, $c, $c);
  $tile→set_icon($tile_image);
  $tile_image→desaturate;
  $tile→set_back_icon($tile_image);
  $tile→post_to_wall;
  meta private;
}
