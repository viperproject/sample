meta version "v2.2,nothing";
meta name "XKCD Comic Viewer";
meta icon "HeartAlt";
meta color "#fffdee00";
// I added navigation controls to this excellent viewer
meta platform "current";

action Save(pic0: Picture) {
  $pic0→save_to_library;
  meta private;
}

action View(comic: Json_Object) returns result: Picture {
  $comic→field("alt")→to_string→post_to_wall;
  $pic := web→download_picture($comic→field("img") ∥ "");
  $pic→post_to_wall;
  $comic→field("title")→to_string→post_to_wall;
  $comic→field("num")→post_to_wall;
  $result := $pic;
}

action Main() {
  $xkcd := "HTTP://www.xkcd.com/";
  $jsonstring := "info.0.json";
  $comic := web→json(web→download($xkcd ∥ $jsonstring));
  $current := $comic→field("num")→to_number;
  $newest := $current;
  $pic := code→View($comic);
  $choices := "Previous,Next,Download,Jump to #,Newest" →split(",");
  while true do {
    $choice := wall→pick_string("Navigate comics", "", $choices);
    wall→clear;
    if $choice = 0 then {
      if $current > 0 then {
        $current := $current - 1;
      }
      $comic := web→json(web→download($xkcd ∥ $current→to_string ∥ "/" ∥ $jsonstring));
      $pic := code→View($comic);
    }
    else {
      if $choice = 1 then {
        if $current < $newest then {
          $current := $current + 1;
        }
        $comic := web→json(web→download($xkcd ∥ $current→to_string ∥ "/" ∥ $jsonstring));
        $pic := code→View($comic);
      }
      else {
        if $choice = 2 then {
          code→Save($pic);
          $comic := web→json(web→download($xkcd ∥ $current→to_string ∥ "/" ∥ $jsonstring));
          $pic := code→View($comic);
        }
        else {
          if $choice = 3 then {
            $jump := wall→ask_number("Comic #");
            wall→clear;
            if(0 < $jump) and ($jump ≤ $newest) then {
              $current := $jump;
              $comic := web→json(web→download($xkcd ∥ $current→to_string ∥ "/" ∥ $jsonstring));
              $pic := code→View($comic);
            }
          }
          else {
            if $choice = 4 then {
              $current := $newest;
              $comic := web→json(web→download($xkcd ∥ $current→to_string ∥ "/" ∥ $jsonstring));
              $pic := code→View($comic);
            }
          }
        }
      }
    }
  }
}

action go() {
  skip;
}

