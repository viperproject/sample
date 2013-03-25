meta version "v2.2,nothing";
meta name "twitter oauth";
//
meta isLibrary "yes";
meta platform "";
meta import oauth {
  pub "xkvn"
  usage {
    action set_consumer_key(consumer_key: String, consumer_secret: String)
    action set_access_token(oauth_token: String, oauth_token_secret: String)
    action create_request(url: String, method: String, params: String_Map) returns request: Web_Request
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

action home_timeline() returns statuses: Json_Object {
  ♻ oauth→set_consumer_key(data→consumer_key, data→consumer_secret);
  ♻ oauth→set_access_token(data→access_token, data→access_token_secret);
  $request := ♻ oauth→create_request("https://api.twitter.com/1/statuses/home_timeline.json", "get", collections→create_string_map);
  $statuses := $request→send→content_as_json;
}

action authorize() {
  ♻ oauth→set_consumer_key(data→consumer_key, data→consumer_secret);
  ♻ oauth→set_access_token("", "");
  $request := ♻ oauth→create_request("https://api.twitter.com/oauth/request_token", "get", collections→create_string_map);
  $d := collections→create_string_map;
  foreach e in $request→send→content→split("&") where true do {
    $strings := $e→split("=");
    $d→set_at($strings→at(0), web→url_decode($strings→at(1)));
  }
  ♻ oauth→set_access_token($d→at("oauth_token"), $d→at("oauth_token_secret"));
  web→browse("https://api.twitter.com/oauth/authorize?oauth_token=" ∥ $d→at("oauth_token"));
  wall→push_new_page;
  $pin := wall→ask_string("enter your pin number");
  $d := collections→create_string_map;
  $d→set_at("oauth_verifier", $pin);
  $request := ♻ oauth→create_request("https://api.twitter.com/oauth/access_token", "get", $d);
  $d := collections→create_string_map;
  foreach e1 in $request→send→content→split("&") where true do {
    $strings1 := $e1→split("=");
    $d→set_at($strings1→at(0), web→url_decode($strings1→at(1)));
  }
  code→set_access_token($d→at("oauth_token"), $d→at("oauth_token_secret"));
  wall→pop_page;
}

action update(status: String) {
  ♻ oauth→set_consumer_key(data→consumer_key, data→consumer_secret);
  ♻ oauth→set_access_token(data→access_token, data→access_token_secret);
  $d := collections→create_string_map;
  $d→set_at("status", $status);
  $request := ♻ oauth→create_request("https://api.twitter.com/1/statuses/update.json", "POST", $d);
  $statuses := $request→send→content_as_json;
}

action set_consumer_key(consumer_key: String, consumer_secret: String) {
  data→consumer_key := $consumer_key;
  data→consumer_secret := $consumer_secret;
}

action set_access_token(access_token: String, access_token_secret: String) {
  data→access_token := $access_token;
  data→access_token_secret := $access_token_secret;
}
var consumer_key : String {
}
var consumer_secret : String {
}
var access_token : String {
}
var access_token_secret : String {
}

