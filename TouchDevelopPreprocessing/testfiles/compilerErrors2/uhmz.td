meta version "v2.2,nothing";
meta name "all things games";
meta icon "documents";
// All the things you need to know about games
meta platform "current";
meta import super_lib {
  pub ""
  usage {
  }
}
meta import data_pic {
  pub "epae"
  usage {
  }
  resolve base_string = ♻base_string with {
  }
  resolve json_core = ♻json_core with {
  }
}
meta import base_string {
  pub "dyeu"
  usage {
  }
}
meta import json_core {
  pub "jtfn"
  usage {
  }
}

action halo_4() {
  "Halo 4: Halo 4 is a new halo game coming soon on November 6, 2012 and I think we are all looking forward to this.\nThey have all new gun graphics (look in photo albums for pics). The multiplayer is all new with graphics, new guns, ordinance drops, new audio, ECT.\nThe new ordinance drops will be featured in the new slayer infinity slayer.\nInfinity is a new ship that master chief is with and it will, as 343 will say, is a gateway to a whole new multiplayer experience.\nThey will also have a new campaign online that will take place a few months after the halo 4 campaign called Spartan ops. The whole process of the campaign could take a range of a little over a month. Because every week they feature a few more missions for you to complete.\nThey have an all new forge mode were you can duplicate objects with the touch of a button along with you can magnate them so you can sick them together nice and evenly so you have no hassle building. They have along with player options so you lay down a highlighted area and you can customize the player options in the field. Also when in player mode there will be shadow effects made better and the buildings will look better as well. So now it will actually, from my perspective, look like you will be there and not playing a game. So lets just say the graphics in everything are ten times better.\nLike I said about the multiplayer is that it feels a little better along with new assassinations for maybe lets say you carrying a flag or an oddball.\nLike I said its all new and tweaked up a little for all new experience and I know to Mitch and I will get more on the way later. But for now I haft to work more on this app.\nI\'ll update later and did I mention there is new level systems and yes I mean systems and now custom load outs and you can possibly customize them during game play.\nLike I said ill update later and bring more info later there is just to Mitch to type later.";
}

event tap_wall_Place(item: Place) {
  skip;
  meta private;
}
var halo_4_war_games_game_type : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?c=hVXscIza&w=320&h=480&i=http%3A%2F%2Fwww.justpushstart.com%2Fwp-content%2Fuploads%2F2012%2F04%2FHalo4New-1.jpg";
}

event tap_wall_Picture_Album(item: Picture_Album) {
  skip;
  meta private;
}
var halo_4_scatershot_promethian_wepon : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?c=VbZmmTQV&w=320&h=480&i=http%3A%2F%2Fwww.thehalofix.com%2Fwp-content%2Fuploads%2F2012%2F06%2FScattershot.png";
}

