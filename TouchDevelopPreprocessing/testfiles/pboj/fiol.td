meta version "v2.2,nothing";
meta name "rmc reader";
meta icon "needle";
meta color "#ff007fff";
// Explores the content of research.microsoft.com.
meta platform "current";

action main() {
  $pic := art→background;
  if colors→is_light_theme then {
    $pic := $pic→clone;
    $pic→invert;
  }
  wall→set_background_picture($pic);
  data→main_tile→set_back_icon(art→tile_background);
  data→main_tile→set_icon(art→tile_background);
  data→cats := collections→create_string_collection;
  code→create_button("search", "");
  data→cats := collections→create_string_collection;
  code→create_button("Publications", "PublicationDate");
  code→create_button("Projects", "DateCreated");
  code→create_button("Videos", "DateRecorded");
}
table nav {
  type = "Decorator";
  keys {
    page: Page
  }
  fields {
    @skip: Number_field
    path: String_field
    term: String_field
    order: String_field
  }
}

action process_messages(rss: Message_Collection) {
  foreach msg in $rss where true do {
    $v := $msg→values;
    $url := $v→at("Url");
    if not $url→is_invalid then {
      $msg→set_link($url);
    }
    $thumbnail := $v→at("Thumbnail");
    if not $thumbnail→is_invalid then {
      $msg→set_picture_link($thumbnail);
    }
    $name := $v→at("Name");
    if not $name→is_invalid then {
      $msg→set_from($name);
    }
    $host := $v→at("MicrosoftHost");
    if not $host→is_invalid then {
      $msg→set_from($host);
    }
    $authors := $v→at("Authors");
    if not $authors→is_invalid then {
      $msg→set_from($authors);
    }
  }
  meta private;
}

action post_next_items() {
  $n := records→Page_decorator→at(wall→current_page);
  $path := $n→path→get;
  if not $path→is_empty then {
    $@skip := $n→@skip→get;
    $@top := 12;
    $orderby := $n→order→get;
    $term := $n→term→get;
    $svc := "http://odata.research.microsoft.com/odata.svc/";
    $url := $svc ∥ $path ∥ "?$skip=" ∥ $@skip ∥ "&$top=" ∥ $@top;
    if not $term→is_empty then {
      $loterm := web→url_encode($term→to_lower_case);
      $url := $url ∥ "&$filter=indexof(tolower(Name),\'" ∥ $loterm ∥ web→url_encode("\') ge 0");
    }
    if not $orderby→is_empty then {
      $url := $url ∥ "&$orderby=" ∥ $orderby ∥ web→url_encode(" desc");
    }
    $s := code→download_with_retry($url);
    if not $s→is_invalid then {
      $feed := web→feed($s);
      code→process_messages($feed);
      $n→@skip→set($@skip + $feed→count);
      $feed→post_to_wall;
    }
  }
  meta private;
}

action push_category_page(path: String, order: String) {
  $page := wall→push_new_page;
  wall→set_title($path);
  wall→set_reversed(true);
  $n := records→Page_decorator→at($page);
  $n→path→set($path);
  $n→@skip→set(0);
  $n→order→set($order);
  code→post_next_items();
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  $t := records→TextBox_decorator→at($item);
  $cat := $t→path→get;
  $order := $t→order→get;
  if $cat→equals("search") then {
    code→push_search_page;
  }
  else {
    code→push_category_page($cat, $order);
  }
  meta private;
}
var background : Picture {
  is\_resource = true;
  url = "https://byfiles.storage.live.com/y1pWExiXDmCAh6yV1RUNnG99lUEtzR7cuJmtrYgsCFRyhV4zbp_BoSOGrw8eXudgX0rc6RLCnNmZDo/hero1rotated.jpg?psid=1";
}

event empty_space_on_wall() {
  code→post_next_items;
  meta private;
}
var main_tile : Tile {
  readonly = true;
}
var tile_background : Picture {
  is\_resource = true;
  url = "https://byfiles.storage.live.com/y1p1kaQNld0lqfSMuMPVw3gQ7YWUndVUAPeJjIb_Kk8pBPBfS2lO3GnqgA7yXNSMdHGKC_D7NorXlY/herotile.png?psid=1";
}

action download_with_retry(url: String) returns s: String {
  $s := invalid→string;
  $retry := 0;
  while $retry < 3 and $s→is_invalid do {
    $s := web→download($url);
    $retry := $retry + 1;
    ... ;
  }
  meta private;
}
var cats : String_Collection {
}

action create_button(cat: String, order: String) {
  $tb1 := wall→create_text_box($cat→to_lower_case, 62);
  $tb1→set_background(colors→transparent);
  $tb1→post_to_wall;
  $t := records→TextBox_decorator→at($tb1);
  data→cats→add($cat);
  $t→path→set($cat);
  $t→order→set($order);
  meta private;
}

action push_search_page() {
  $page := wall→push_new_page;
  wall→set_title("search");
  $cati := wall→pick_string("Pick a category", "", data→cats);
  data→search_cat := data→cats→at($cati);
  data→search_term := wall→ask_string("What are you searching for?");
  wall→add_button("favs.addto", "pin to start");
  code→push_search($page);
  meta private;
}
table tb {
  type = "Decorator";
  keys {
    target: TextBox
  }
  fields {
    order: String_field
    path: String_field
  }
}
var search_cat : String {
}
var search_term : String {
}

action push_search(page: Page) {
  $path := data→search_cat;
  $sub_title := data→search_cat ∥ " with " ∥ data→search_term;
  wall→set_subtitle($sub_title);
  wall→set_reversed(true);
  wall→clear;
  $n := records→Page_decorator→at($page);
  $n→path→set($path);
  $n→term→set(data→search_term);
  $n→@skip→set(0);
  code→post_next_items();
  meta private;
}

action search() {
  $page := wall→push_new_page;
  wall→set_title("search");
  code→push_search($page);
}
var search_tile : Tile {
  readonly = true;
}

event tap_wall_Page_Button(item: Page_Button) {
  $page := $item→page;
  $n := records→Page_decorator→at($page);
  if not $n→term→get→is_invalid then {
    code→pin_search_tile;
  }
  meta private;
}

action pin_search_tile() {
  data→search_tile→set_icon(art→tile_background);
  data→search_tile→set_title("search");
  data→search_tile→set_back_title("search");
  data→search_tile→pin_to_start;
  meta private;
}

