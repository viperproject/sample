meta version "v2.2,nothing";
meta name "twitter client sample";
//
meta platform "";
meta import twitter_oauth {
  pub "cnsu"
  usage {
    action authorize()
    action update(status: String)
    action set_consumer_key(consumer_key: String, consumer_secret: String)
    action set_access_token(access_token: String, access_token_secret: String)
  }
  resolve oauth = ♻oauth with {
  }
  resolve hmac = ♻hmac with {
  }
  resolve sha1 = ♻sha1 with {
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
  resolve epoch_util = ♻epoch_util with {
  }
}
meta import oauth {
  pub "xkvn"
  usage {
  }
  resolve hmac = ♻hmac with {
  }
  resolve sha1 = ♻sha1 with {
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
  resolve epoch_util = ♻epoch_util with {
  }
}
meta import hmac {
  pub "huqv"
  usage {
  }
  resolve sha1 = ♻sha1 with {
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import sha1 {
  pub "frzd"
  usage {
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import collection_util {
  pub "auvya"
  usage {
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import binary_operators {
  pub "juwd"
  usage {
  }
}
meta import epoch_util {
  pub "kokf"
  usage {
  }
}

action main() {
  "communicating..." →post_to_wall;
  ♻ twitter_oauth→set_consumer_key("T5bwTycqfENnvwJSNVh7Q", "az0nUArv6acl9GFPFqN0mYpv91aEYsOvnjYi2Cqrk");
  ♻ twitter_oauth→set_access_token("", "");
  ♻ twitter_oauth→authorize;
  wall→clear;
  while true do {
    $s := wall→ask_string("what are you doing?");
    ♻ twitter_oauth→update($s);
    wall→clear;
  }
}

