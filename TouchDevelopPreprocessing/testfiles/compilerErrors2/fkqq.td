meta version "v2.2,nothing";
meta name "52 Card Pickup";
meta icon "info";
meta color "#ff9955bb";
// See how easy it is to use playing cards in your script!
//
// Add a reference to the "playing card deck" library, currently /iush, and all the other libraries are taken care of.  After two calls to get things started, shuffle the deck and start dealing!
meta platform "current";
meta import deck {
  pub "iush"
  usage {
    action shuffle()
    action deal() returns new_card: Json_Object
    action count() returns i: Number
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
    action to_string(c: Json_Object) returns s: String
  }
  resolve suits = ♻suits with {
  }
  resolve definition = ♻definition with {
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection resolve instance = ♻instance with {
  }
  resolve color_helpers = ♻color_helpers}

action main() {
  ♻ definition→init;
  ♻ card→init;
  ♻ deck→shuffle;
  while♻ deck→count > 0 do {
    $random_card := ♻ deck→deal;
    ♻ card→to_string($random_card)→post_to_wall;
  }
}

