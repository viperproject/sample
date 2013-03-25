meta version "v2.2,nothing";
meta name "twitter client sample";
//
meta platform "";
meta import twitter_oauth {
  pub "vxcga"
  usage {
    action authorize()
    action update(status: String)
    action set_consumer_key(consumer_key: String, consumer_secret: String)
    action set_access_token(access_token: String, access_token_secret: String)
  }
  resolve oauth = ♻oauth with {
  }
  resolve sha1 = ♻sha1 with {
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
  resolve hmac = ♻hmac with {
  }
  resolve epoch_util = ♻epoch_util with {
  }
}
meta import oauth {
  pub "ntbn"
  usage {
  }
  resolve sha1 = ♻sha1 with {
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
  resolve hmac = ♻hmac with {
  }
  resolve epoch_util = ♻epoch_util with {
  }
}
meta import sha1 {
  pub "bqnm"
  usage {
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import collection_util {
  pub "ledg"
  usage {
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import binary_operators {
  pub "vhql"
  usage {
  }
}
meta import hmac {
  pub "lhxxb"
  usage {
  }
  resolve sha1 = ♻sha1 with {
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import epoch_util {
  pub "ivoq"
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

