meta version "v2.2,nothing";
meta name "SimpleSearch";
meta icon "Binoculars";
meta color "#ff008080";
//
meta platform "current";

action main() {
  records→searchResults_table→clear;
  $e := records→searchResults_table→add_row;
  $e→Title→set("a");
  $e→Url→set("b");
  $e→DisplayUrl→set("c");
}
table searchResults {
  type = "Table";
  fields {
    Title: String_field
    Url: String_field
    DisplayUrl: String_field
  }
}