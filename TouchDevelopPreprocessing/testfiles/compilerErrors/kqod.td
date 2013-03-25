meta version "v2.2,nothing";
meta name "shake jeet";
meta color "#ffa52a2a";
// Shake song
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}
table thing {
  type = "Decorator";
  keys {
    target: Json_Object
  }
}

event shake() {
  media→picture_albums→post_to_wall;
  media→song_albums→random→songs→random→play;
  meta private;
}

