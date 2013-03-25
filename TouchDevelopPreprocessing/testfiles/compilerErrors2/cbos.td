meta version "v2.2,nothing";
meta name "52 Card Pickup";
meta icon "info";
meta color "#ff9955bb";
// See how easy it is to use playing cards in your script!
//
// Add a reference to the "playing card deck" library, currently /jvzu, and all the other libraries are taken care of.  Then just shuffle the deck and start dealing!
meta platform "current";
meta import deck {
  pub "jvzu"
  usage {
    action shuffle()
    action deal() returns new_card: Json_Object
    action count() returns i: Number
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
    action to_string(c: Json_Object) returns s: String
  }
  resolve suits = ♻suits with {
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection resolve instance = ♻instance with {
  }
  resolve color_helpers = ♻color_helpers resolve definition = ♻definition with {
  }
}

action main() {
  ♻ deck→shuffle;
  while♻ deck→count > 0 do {
    $random_card := ♻ deck→deal;
    ♻ card→to_string($random_card)→post_to_wall;
  }
}

