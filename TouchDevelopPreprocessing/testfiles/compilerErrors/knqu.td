meta version "v2.2,nothing";
meta name "Shaker";
meta icon "music";
meta color "#ff9955bb";
// Shake your phone for a random song.
meta platform "current,media";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

event shake() {
  media→songs→random→play;
  meta private;
}
var a : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?c=zZDvYHFI&w=320&h=480&i=http%3A%2F%2Flh3.ggpht.com%2F-QPA7DmGBF80%2FUB-Ey2M4kTI%2FAAAAAAAAAA8%2FbHuyMokqfA0%2F111684-glowing-green-neon-icon-media-music-sixteenth-note.png";
}
table thing {
  type = "Object";
}

event tap_wall_Picture(item: Picture) {
  player→pause;
  meta private;
}

