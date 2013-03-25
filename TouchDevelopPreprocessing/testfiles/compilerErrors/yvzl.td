meta version "v2.2,nothing";
meta name "Pin website icon+";
meta icon "heart";
meta color "#ffff7518";
// Pin website icon script extended to support customized icons for sites.
meta platform "current";

action main() {
  data→link_8_tile→post_to_wall;
  data→link_7_tile→post_to_wall;
  data→link_6_tile→post_to_wall;
  data→link_5_tile→post_to_wall;
  data→link_4_tile→post_to_wall;
  data→link_3_tile→post_to_wall;
  data→link_2_tile→post_to_wall;
  data→link_1_tile→post_to_wall;
  wall→create_text_box("Tap any of the tiles below to enter website address and pin the tile to start.", 18)→post_to_wall;
}

action link_1() {
  code→go(data→link_1_tile);
}

action link_2() {
  code→go(data→link_2_tile);
}
var link_2_tile : Tile {
  readonly = true;
}

action link_3() {
  code→go(data→link_3_tile);
}

action link_4() {
  code→go(data→link_4_tile);
}

action link_5() {
  code→go(data→link_5_tile);
}

action link_6() {
  code→go(data→link_6_tile);
}

action go(tile: Tile) {
  $req_url := records→Tile_decorator→at($tile)→url→get;
  if not $req_url→is_match_regex("^http(s)?:") then {
    $req_url := "http://" ∥ $req_url;
  }
  web→browse($req_url);
  time→stop_and_close;
  meta private;
}
var link_3_tile : Tile {
  readonly = true;
}
var link_4_tile : Tile {
  readonly = true;
}
var link_5_tile : Tile {
  readonly = true;
}
var link_6_tile : Tile {
  readonly = true;
}

action link_7() {
  code→go(data→link_7_tile);
}

action link_8() {
  code→go(data→link_8_tile);
}
var link_7_tile : Tile {
  readonly = true;
}
var link_8_tile : Tile {
  readonly = true;
}

event tap_wall_Tile(tile: Tile) {
  wall→clear;
  ... ;
  $url := "";
  $t := "";
  while $url→is_empty do {
    $url := wall→ask_string("Enter or paste website link");
  }
  $pic, $s, $precomposed := code→fetch_details($url);
  if $pic→is_invalid then {
    if wall→ask_boolean("No suitable icon found for the site. Would you still like to pin it?", "If you do it will be pinned to start with a blank icon.") then {
      if wall→ask_boolean("Would you like to use custom URL for icon instead?", "") then {
        $pic := code→custom_icon();
      }
      else {
        $pic := media→create_picture($tile→width, $tile→height);
      }
      $t := $url;
    }
  }
  if not $pic→is_invalid then {
    if $precomposed then {
      $tile→set_background(colors→background);
    }
    else {
      $tile→set_background(colors→accent);
    }
    if $pic→width > $tile→width or $pic→height > $tile→height then {
      $pic→resize($tile→width, - 1);
    }
    else {
      if $pic→width < $tile→width or $pic→height < $tile→height then {
        if wall→ask_boolean("Would you like the icon to be stretched?", "It will be centered otherwise with tile background becoming transparent.") then {
          $pic→resize($tile→width, - 1);
        }
        $tile→set_background(colors→background);
        $t := $url;
      }
    }
    records→Tile_decorator→at($tile)→url→set($url);
    records→Tile_decorator→at($tile)→text→set($s);
    if $url→is_match_regex("^http(s)?:\\/\\/.*") then {
      $url := $url→replace_regex("^http(s)?:\\/\\/", "");
    }
    if $url→count > 14 then {
      $url := $url→substring(0, 13) ∥ "…";
    }
    if $s→count > 40 then {
      $s := $s→substring(0, 39) ∥ "…";
    }
    if $t→is_match_regex("^http(s)?:\\/\\/.*") then {
      $t := $t→replace_regex("^http(s)?:\\/\\/", "");
    }
    if $t→count > 14 then {
      $t := $t→substring(0, 13) ∥ "…";
    }
    $tile→set_icon($pic);
    $tile→set_content($s);
    $tile→set_title($t);
    $tile→set_back_title($url);
    $back := media→create_picture($tile→width, $tile→height);
    $back→clear(colors→accent);
    $tile→set_back_icon($back);
    $tile→pin_to_start;
  }
  time→stop;
  meta private;
}

action fetch_details(url: String) returns icon: Picture, title: String, precomposed: Boolean {
  $req_url := $url;
  if not $url→is_match_regex("^http(s)?:") then {
    $req_url := "http://" ∥ $url;
  }
  $response := web→create_request($req_url)→send;
  $precomposed := false;
  $icon := invalid→picture;
  $icons := code→find_icon_urls($req_url, $response);
  $title := code→grep_title($url, $response);
  $plain_icons := collections→create_string_collection;
  $precomposed_icons := collections→create_string_collection;
  foreach k in $icons→keys where true do {
    if $icons→at($k)→equals("precomposed") then {
      $precomposed_icons→add($k);
    }
    else {
      $plain_icons→add($k);
    }
  }
  $plain_icon := code→find_largest_icon($plain_icons);
  $found := false;
  if not $plain_icon→is_invalid then {
    if $plain_icon→width ≥ 100 then {
      $found := true;
      $icon := $plain_icon;
    }
  }
  if not $found then {
    $precomposed_icon := code→find_largest_icon($precomposed_icons);
    if $plain_icon→is_invalid then {
      $icon := $precomposed_icon;
      $precomposed := not $precomposed_icon→is_invalid;
    }
    else {
      if $precomposed_icon→is_invalid then {
        $icon := $plain_icon;
      }
      else {
        $precomposed := $plain_icon→width < $precomposed_icon→width;
        if $plain_icon→width < $precomposed_icon→width then {
          $icon := $precomposed_icon;
        }
        else {
          $icon := $plain_icon;
        }
      }
    }
  }
  meta private;
}

action grep_hrefs(content: String, pattern: String) returns urls: String_Collection {
  $urls := collections→create_string_collection;
  $href_pattern := "\\A.*([hH][rR][eE][fF]\\s*=\\s*\"([^\"]*)\"|[hH][rR][eE][fF]\\s*=\\s*\'([^\']*)\').*\\z";
  foreach s in $content→matches($pattern) where true do {
    $h := $s→replace_regex($href_pattern, "$2$3");
    if not $h→is_empty and not $h→equals($s) then {
      $urls→add($h);
    }
  }
  meta private;
}

action append_url(base_url: String, url: String) returns s: String {
  if $url→starts_with("//") then {
    $url := $base_url→substring(0, $base_url→index_of(":", 0) + 1) ∥ $url;
  }
  else {
    if $url→starts_with("/") then {
      $url := $base_url→replace_regex("^([^:]*:/*[^/]+)/.*$", "$1") ∥ $url;
    }
    else {
      $url := $base_url→replace_regex("^(.*)((/.*)|(\\?.*))?$", "$1") ∥ $url;
    }
  }
  $s := $url;
  meta private;
}
var link_1_tile : Tile {
  readonly = true;
}

action download_ico(url: String) returns icon: Picture {
  // Obsolete. Converts an ico image to omg using a separate web service.
  $icon := invalid→picture;
  $url := code→follow_redirects($url);
  $coverter_site := "http://www.convertico.com";
  if not $url→is_invalid then {
    $converter_url := $coverter_site ∥ "/appleJax.php";
    $boundary := "-----------------------------123456789098765";
    $field := "--" ∥ $boundary ∥ "\nContent-Disposition: form-data; name=";
    $r := web→create_request($converter_url);
    $r→set_method("post");
    $content := $field ∥ "\"remoteimgfile\"\n\n" ∥ $url ∥ "\n" ∥ $field ∥ "\"selected\"\n\nremote\n" ∥ "\n--" ∥ $boundary ∥ "--\n\n";
    $r→set_content($content);
    $r→set_header("Content-Type", "multipart/form-data; boundary=" ∥ $boundary);
    $response := $r→send;
    $json := $response→content_as_json;
    if $json→is_invalid then {
      time→sleep(3);
      $response := $r→send;
      $json := $response→content_as_json;
    }
    if $json→is_invalid then {
      time→sleep(3);
      $response := $r→send;
      $json := $response→content_as_json;
    }
    if not $json→is_invalid then {
      if $json→contains_key("New") then {
        $icon := web→download_picture($coverter_site ∥ $json→string("New"));
      }
    }
  }
  meta private;
}

action follow_redirects(original: String) returns url: String {
  $url := $original;
  $seen := collections→create_string_collection;
  $seen→add($url);
  $x := 0;
  while $x ≠ 200 and not $url→is_invalid do {
    $request := web→create_request($url);
    $response := $request→send;
    $x := $response→status_code;
    if $x ≥ 300 and $x < 400 then {
      $url := $response→header("Location");
      if $seen→contains($url) then {
        $x := - 1;
      }
      $seen→add($url);
    }
    if $x ≥ 400 or $x < 200 then {
      $url := invalid→string;
    }
  }
  meta private;
}

action update_tile(tile: Tile, title: String, icon: Picture, url: String) {
  // Obsolete. Previously used together with ico favicons
  //
  $buf := "";
  $strings := collections→create_string_collection;
  $matches := $title→matches("\\w+\\S*\\w*\\s*");
  $w := 8;
  foreach s in $matches where true do {
    $t := $s→trim(" \t\n");
    if not $t→is_empty then {
      $n := $buf ∥ $t ∥ " ";
      if $n→count > $w and $buf→count > 3 then {
        $strings→add($buf→trim(" "));
        $buf := $t ∥ " ";
        $w := 10;
      }
      else {
        $buf := $n;
      }
    }
  }
  $strings→add($buf→trim(" "));
  //
  $pic := media→create_picture(160, 160);
  $icon→resize(32, - 1);
  $y := 30;
  $pic→blend($icon, 0, $y - 8, 0, 1);
  $x := $icon→width + 3;
  $count := 0;
  foreach s1 in $strings where $count < 4 do {
    $pic→draw_text($x, $y, $s1, 24, 0, colors→foreground);
    $y := $y + 27;
    $x := 5;
    $count := $count + 1;
  }
  //
  $tile→set_background(colors→accent);
  $tile→set_icon($pic);
  $tile→set_content("");
  $tile→set_back_title($url);
  meta private;
}

action find_icon_urls(url: String, response: Web_Response) returns urls: String_Map {
  $urls := collections→create_string_map;
  $s := $response→content;
  //
  $hrefs := collections→create_string_map;
  $strings := code→grep_hrefs($s, "<[lL][iI][nN][kK]\\s[^>]*([hH][rR][eE][fF]\\s*=\\s*\"[^\"]*\"|[hH][rR][eE][fF]\\s*=\\s*\'[^\']*\')?[^>]*([rR][eE][lL]\\s*=\\s*\"[^\"]*apple-touch-icon-precomposed[^\"]*\"|[rR][eE][lL]\\s*=\\s*\'[^\']*apple-touch-icon-precomposed[^\']*\')[^>]*([hH][rR][eE][fF]\\s*=\\s*\"[^\"]*\"|[hH][rR][eE][fF]\\s*=\\s*\'[^\']*\')?");
  $strings→add("/apple-touch-icon-57x57-precomposed.png");
  $strings→add("/apple-touch-icon-72x72-precomposed.png");
  $strings→add("/apple-touch-icon-114x114-precomposed.png");
  $strings→add("/apple-touch-icon-144x144-precomposed.png");
  $strings→add("/apple-touch-icon-precomposed.png");
  foreach s1 in $strings where true do {
    $hrefs→set_at($s1, "precomposed");
  }
  //
  $strings := code→grep_hrefs($s, "<[lL][iI][nN][kK]\\s[^>]*([hH][rR][eE][fF]\\s*=\\s*\"[^\"]*\"|[hH][rR][eE][fF]\\s*=\\s*\'[^\']*\')?[^>]*([rR][eE][lL]\\s*=\\s*\"apple-touch-icon\"|[rR][eE][lL]\\s*=\\s*\'apple-touch-icon\')[^>]*([hH][rR][eE][fF]\\s*=\\s*\"[^\"]*\"|[hH][rR][eE][fF]\\s*=\\s*\'[^\']*\')?");
  $strings→add_many(code→grep_hrefs($s, "<[lL][iI][nN][kK]\\s[^>]*([hH][rR][eE][fF]\\s*=\\s*\"[^\"]*\"|[hH][rR][eE][fF]\\s*=\\s*\'[^\']*\')?[^>]*([rR][eE][lL]\\s*=\\s*\"icon\"|[rR][eE][lL]\\s*=\\s*\'icon\')[^>]*([hH][rR][eE][fF]\\s*=\\s*\"[^\"]*\"|[hH][rR][eE][fF]\\s*=\\s*\'[^\']*\')?"));
  $strings→add("/apple-touch-icon-57x57.png");
  $strings→add("/apple-touch-icon-72x72.png");
  $strings→add("/apple-touch-icon-114x114.png");
  $strings→add("/apple-touch-icon-144x144.png");
  $strings→add("/apple-touch-icon.png");
  foreach s3 in $strings where true do {
    if not $hrefs→keys→contains($s3) then {
      $hrefs→set_at($s3, "");
    }
  }
  //
  foreach k in $hrefs→keys where true do {
    if not $k→is_match_regex("\\.[iI][cC][oO](\\?.*)?\\Z") then {
      if not $k→is_match_regex("^http(s)?:/.*") then {
        $urls→set_at(code→append_url($url, $k), $hrefs→at($k));
      }
      else {
        $urls→set_at($k, $hrefs→at($k));
      }
    }
  }
  meta private;
}

action grep_title(url: String, response: Web_Response) returns title: String {
  $title := $url;
  $ss := $response→content→matches("<[tT][iI][tT][lL][eE]>[^>]*<");
  if $ss→count ≠ 0 then {
    $title := web→html_decode($ss→at(0)→replace_regex("\\A.*>(.*)<\\z", "$1"));
  }
  meta private;
}

action find_largest_icon(urls: String_Collection) returns icon: Picture {
  $icon := invalid→picture;
  $not_found := true;
  foreach s in $urls where $not_found do {
    $after_redir := code→follow_redirects($s);
    if not $after_redir→is_invalid then {
      $pic := invalid→picture;
      $response := web→create_request($after_redir)→send;
      $t := $response→header("Content-Type");
      if $response→status_code = 200 and $t→contains("image") and not $t→contains("ico") then {
        $pic := $response→content_as_picture;
      }
      if not $pic→is_invalid then {
        if $icon→is_invalid then {
          $icon := $pic;
        }
        else {
          if $pic→width > $icon→width then {
            $icon := $pic;
          }
        }
        $not_found := $not_found and $pic→width < 100;
      }
    }
  }
  meta private;
}
table thing {
  type = "Decorator";
  keys {
    target: Tile
  }
  fields {
    url: String_field
    text: String_field
  }
}

action custom_icon() returns pic: Picture {
  $url := "";
  while $url→is_empty do {
    $url := wall→ask_string("Input or paste icon URL (JPEG or PNG) ");
  }
  $pic := web→download_picture($url);
  if $pic→is_invalid then {
    wall→prompt("Failed to download custom icon.");
    time→stop;
  }
  meta private;
}

action go2() {
  skip;
  meta private;
}

