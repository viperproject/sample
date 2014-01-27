// Extracted minimal code from wbxsa that causes sample crash (has to do with InvalidExpression)

action main() {
  $board := media→create_full_board;
  data→ss := $board→create_sprite_set;
  $pic := media->create_picture(10,10);
  $sprite := $board→create_picture($pic);
  data→ss→add($sprite);

  $sprite2 := data->ss->at(0);
  data→ss→at(0)->move(10, 10);
}

var ss : Sprite_Set {
  transient = true;
}
