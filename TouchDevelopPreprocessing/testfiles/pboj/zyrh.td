meta version "v2.2,nothing";
meta name "home controller";
meta icon "home";
// An easy way to control your tv with TouchDevelop.
meta platform "current";

action main() {
  // Posts your favorite server and player on the wall to explore and control them.
  // If the server is not chosen yet, pick one.
  if data→server→is_invalid then {
    data→server := home→choose_server;
  }
  // If the player is not chosen yet, pick one.
  if data→pa→is_invalid then {
    data→pa := home→choose_player;
  }
  data→server→post_to_wall;
  data→pa→post_to_wall;
}
var server : Media_Server {
}
var pa : Media_Player {
}

