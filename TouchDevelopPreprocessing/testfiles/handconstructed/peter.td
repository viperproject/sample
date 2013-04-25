meta version "v2.2,nothing";
meta name "SimpleSearch";
meta icon "Binoculars";
meta color "#ff008080";
//
meta platform "current";

action main() {
  $query := "https://api.datamarket.azure.com/Bing/Search/Web?Query=\'sushi\'&$format=JSON";
  $accountKey := art→API_key;
  $request := web→create_request($query);
  $request→set_credentials($accountKey, $accountKey);
  $response := $request→send→content_as_json;
  $content := $response→field("d")→field("results");
  records→searchResults_table→clear;
  for 0 ≤ j < 50 do {
    $e := records→searchResults_table→add_row;
    $e→Title→set($content→at($j)→field("Title")→to_string);
    $e→Url→set($content→at($j)→field("Url")→to_string);
    $e→DisplayUrl→set($content→at($j)→field("DisplayUrl")→to_string);
  }
  code→show;
}
table searchResults {
  type = "table";
  fields {
    Title: String_field
    Url: String_field
    DisplayUrl: String_field
  }
}

action show() {
  if box→is_init then {
    wall→set_foreground(colors→white);
  }
  if true then {
    foreach e2 in records→searchResults_table where true do {
      skip;
      do box {
        box→set_background(colors→green→darken(0.5));
        box→set_padding(0.2, 0.2, 0.2, 0.2);
        box→set_margins(0, 0, 0.2, 0);
        $e2→Title→get→post_to_wall;
        $e2→DisplayUrl→get→post_to_wall;
        box→on_tapped($handler);
        where handler() {
          web→browse($e2→Url→get);
        }
      }
    }
  }
}
var API_key : String {
  is\_resource = true;
  url = "apikey:http://www.bing.com/developers/";
}
