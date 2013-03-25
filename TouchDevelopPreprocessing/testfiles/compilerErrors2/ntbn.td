meta version "v2.2,nothing";
meta name "oauth";
//
meta isLibrary "yes";
meta platform "";
meta import sha1 {
  pub "bqnm"
  usage {
    action calculate(blob: Number_Collection) returns hash: Number_Collection
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import collection_util {
  pub "ledg"
  usage {
    action from_string(s: String) returns v: Number_Collection
    action to_hex_string(v: Number_Collection) returns s: String
    action to_string(v: Number_Collection) returns s: String
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
    action sha1_hmac(dat: Number_Collection, key: Number_Collection) returns v: Number_Collection
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
    action get_epoch_time() returns epoch_time: Number
  }
}

action set_consumer_key(consumer_key: String, consumer_secret: String) {
  if data→credentials→is_invalid then {
    data→credentials := collections→create_string_map;
  }
  data→credentials→set_at("consumer key", $consumer_key);
  data→credentials→set_at("consumer secret", $consumer_secret);
}

action create_signature(url: String, method: String, params: String_Map) {
  if data→credentials→is_invalid then {
    data→credentials := collections→create_string_map;
  }
  $strings := collections→create_string_collection;
  data→parameters→set_at("oauth_consumer_key", data→credentials→at("consumer key"));
  if data→credentials→keys→contains("oauth token") and not data→credentials→at("oauth token")→is_empty then {
    data→parameters→set_at("oauth_token", data→credentials→at("oauth token"));
  }
  $d := collections→create_string_map;
  $d→set_many($params);
  $d→set_many(data→parameters);
  // concat data
  $key := code→url_encode(data→credentials→at("consumer secret")) ∥ ("&") ∥ code→url_encode(data→credentials→at("oauth token secret"));
  $data\_ := $method→to_upper_case ∥ "&" ∥ code→url_encode($url) ∥ "&";
  $query := "";
  foreach e in code→sort_and_make_pair($d) where true do {
    if not $query→is_empty then {
      $query := $query ∥ "&";
    }
    $query := $query ∥ $e;
  }
  $data\_ := $data\_ ∥ code→url_encode($query);
  $v := ♻ hmac→sha1_hmac(♻ collection_util→from_string($data\_), ♻ collection_util→from_string($key));
  $signature := web→base64_encode(♻ collection_util→to_string($v));
  data→parameters→set_at("oauth_signature", $signature);
}
var credentials : String_Map {
}

action initialize() {
  data→parameters := collections→create_string_map;
  data→parameters→set_at("oauth_timestamp", ♻ epoch_util→get_epoch_time→to_string);
  data→parameters→set_at("oauth_nonce", ♻ epoch_util→get_epoch_time→to_string ∥ ♻ collection_util→to_hex_string(♻ sha1→calculate(♻ collection_util→from_string(math→random(16777216)→to_string))));
  data→parameters→set_at("oauth_signature_method", "HMAC-SHA1");
  data→parameters→set_at("oauth_version", "1.0");
  meta private;
}
var parameters : String_Map {
}

action set_access_token(oauth_token: String, oauth_token_secret: String) {
  if data→credentials→is_invalid then {
    data→credentials := collections→create_string_map;
  }
  data→credentials→set_at("oauth token", $oauth_token);
  data→credentials→set_at("oauth token secret", $oauth_token_secret);
}

action url_encode(s: String) returns s2: String {
  $s2 := "";
  foreach e in web→url_encode($s) where true do {
    if $e→compare("!") = 0 then {
      $s2 := $s2 ∥ "%21";
    }
    else {
      if $e→compare("@") = 0 then {
        $s2 := $s2 ∥ "%40";
      }
      else {
        if $e→compare("#") = 0 then {
          $s2 := $s2 ∥ "%23";
        }
        else {
          if $e→compare("$") = 0 then {
            $s2 := $s2 ∥ "%24";
          }
          else {
            if $e→compare("&") = 0 then {
              $s2 := $s2 ∥ "%26";
            }
            else {
              if $e→compare("(") = 0 then {
                $s2 := $s2 ∥ "%28";
              }
              else {
                if $e→compare(")") = 0 then {
                  $s2 := $s2 ∥ "%29";
                }
                else {
                  if $e→compare("*") = 0 then {
                    $s2 := $s2 ∥ "%2A";
                  }
                  else {
                    if $e→compare("+") = 0 then {
                      $s2 := $s2 ∥ "%2B";
                    }
                    else {
                      if $e→compare(":") = 0 then {
                        $s2 := $s2 ∥ "%3A";
                      }
                      else {
                        if $e→compare("?") = 0 then {
                          $s2 := $s2 ∥ "%3F";
                        }
                        else {
                          if $e→compare("=") = 0 then {
                            $s2 := $s2 ∥ "%3D";
                          }
                          else {
                            if $e→compare(";") = 0 then {
                              $s2 := $s2 ∥ "%3B";
                            }
                            else {
                              if $e→compare("\'") = 0 then {
                                $s2 := $s2 ∥ "%27";
                              }
                              else {
                                if $e→compare(",") = 0 then {
                                  $s2 := $s2 ∥ "%2C";
                                }
                                else {
                                  if $e→compare("/") = 0 then {
                                    $s2 := $s2 ∥ "%2F";
                                  }
                                  else {
                                    $s2 := $s2 ∥ $e;
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  meta private;
}

action sort_and_make_pair(d: String_Map) returns strings: String_Collection {
  $strings := collections→create_string_collection;
  $keys := $d→keys;
  $keys→sort;
  foreach e in $keys where true do {
    $strings→add($e ∥ "=" ∥ code→url_encode($d→at($e)));
  }
  meta private;
}

action get_authorization() returns s: String {
  $s := "";
  foreach e in code→sort_and_make_pair(data→parameters) where true do {
    if not $s→is_empty then {
      $s := $s ∥ ",";
    }
    $s := $s ∥ $e;
  }
  $s := "OAuth " ∥ $s;
}

action get_query(d: String_Map) returns s: String {
  $s := "";
  foreach e in code→sort_and_make_pair($d) where true do {
    if not $s→is_empty then {
      $s := $s ∥ "&";
    }
    $s := $s ∥ $e;
  }
  meta private;
}

action create_request(url: String, method: String, params: String_Map) returns request: Web_Request {
  code→initialize;
  code→create_signature($url, $method, $params);
  if $method→to_lower_case→compare("get") = 0 then {
    $url := $url ∥ "?" ∥ code→get_query($params);
  }
  $request := web→create_request($url);
  $request→set_method($method→to_lower_case);
  $request→set_header("Authorization", code→get_authorization);
  if $method→to_lower_case→compare("post") = 0 then {
    $request→set_content(code→get_query($params));
    $request→set_header("Content-Type", "application/x-www-form-urlencoded");
  }
}

