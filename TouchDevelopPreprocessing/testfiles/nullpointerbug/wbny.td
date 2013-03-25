meta version "v2.2,nothing";
meta name "kevcoder.ui";
meta icon "controls";
// 0.0.1 UI stuff
meta isLibrary "yes";
meta platform "current";

action buildListItem(text: String, value: String, fontSize: Number, page: Page) {
  if $fontSize ≤ 19 or $fontSize→is_invalid then {
    $fontSize := 19;
  }
  $tBox := wall→create_text_box($text, $fontSize);
  $tBox→post_to_wall;
  meta private;
}

action buildList(items: String_Map, fontSize: Number) {
  $ddlPg := wall→push_new_page;
  foreach s in $items where true do {
    code→buildListItem($s, $items→at($s), $fontSize, $ddlPg);
    data→ListItemData := $items;
  }
}

action buildListItemTest() {
  $numA := "A" →to_unicode;
  $col := collections→create_string_map;
  // Build a collection of ListItems w/ capital letter as text & Unicode as value
  $i := 25;
  while $i ≥ 0 do {
    $x := $numA + $i;
    $col→set_at($x→to_character, $x→to_string);
    $i := $i - 1;
  }
  // Now run the actual test
  code→buildList($col, 30);
}

action listItem\_OnTap(t: TextBox) returns selectedValue: String {
  wall→pop_page;
  $selectedValue := data→ListItemData→at($t→text);
}
var listItemValue : String {
}
var ListItemData : String_Map {
}

