meta version "v2.2,nothing";
meta name "translate for me";
meta icon "recycle";
meta color "#ff00008b";
// Using Bing Translator to translate
// between 37 languages.
// Internet connection required.
// Push arrows left for autodiscover
// source language and translate to
// phone current language.
// Use arrows right to translate
// between highlighted languages.
// Tap results for text to speech conversions. (supported languages only)
// V2.0 beter Colors, speech and
// scroll menu.
meta platform "current";

action main() {
  wall→clear;
  if data→isLightTheme→is_invalid then {
    data→isLightTheme := colors→is_light_theme;
    data→init_done := false;
  }
  else {
    if not data→isLightTheme→equals(colors→is_light_theme) then {
      data→init_done := false;
      data→isLightTheme := colors→is_light_theme;
    }
    else {
      data→isLightTheme := colors→is_light_theme;
    }
  }
  wall→set_title("translator");
  wall→add_button("transport.rew", "auto");
  wall→add_button("transport.ff", "translate");
  data→page_main := wall→current_page;
  code→create_langNameMap;
  code→create_board;
  code→poste_lang_menu;
  data→init_done := true;
}

action create_lang_Menues(sprite_set: Sprite_Set, x_offset: Number, color: Color) {
  $light := colors→is_light_theme;
  $cFont := colors→foreground;
  if $light then {
    $cFont := colors→background;
  }
  $n := data→languageNames→count;
  $dim_x := 228;
  $dim_y := 79;
  $pic := media→create_picture($dim_x, $dim_y);
  $pic→fill_rect(0, 0, $dim_x, $dim_y, 0, $color);
  for 0 ≤ i1 < $n do {
    $s := data→languageNames→keys→at($i1);
    $name := data→languageNames→at($s);
    $pic1 := $pic→clone;
    $pic1→draw_text(5, $dim_y / 2 - 14, $name, 28, 0, $cFont);
    $sprite := data→board→create_picture($pic1);
    $sprite→set_friction(0.02);
    $sprite→set_opacity(0.7);
    $sprite→set_x($x_offset + $dim_x / 2);
    $sprite_set→add($sprite);
    foreach sprite1 in $sprite_set where true do {
      $sprite1→move(0, - 90);
    }
  }
  foreach sprite2 in $sprite_set where true do {
    $sprite2→move(0, 360);
  }
  meta private;
}

action create_langNameMap() {
  data→languageNames := collections→create_string_map;
  $langs := "ar,bg,ca,cs,da,de,el,en,es,et,fa,fi,fr,he,hu,ht,id,it,ja,ko,lt,lv,mww,no,nl,pl,pt,ro,ru,sk,sl,sv,th,tr,uk,vi,zh-CHS,zh-CHT" →split(",");
  $n := $langs→count;
  $langNames := "Arabic,Bulgarian,Catalan,Czech,Danish,German,Greek,English,Spanish,Estonian,Persian,Finnish,French,Hebrew,Hungarian,Haitian Creole,Indonesian,Italian,Japanese,Korean,Lithuanian,Latvian,Hmong Daw,Norwegian,Dutch,Polish,Portuguese,Romanian,Russian,Slovak,Slovenian,Swedish,Thai,Turkish,Ukrainian,Vietnamese,Chinese (Simpl.),Chinese (Trad.)" →split(",");
  for 0 ≤ i < $n do {
    $n1 := $langs→at($i);
    $N := $langNames→at($i);
    data→languageNames→set_at($n1, $N);
  }
  meta private;
}
var languageNames : String_Map {
}
var board : Board {
}
var lSpr_set : Sprite_Set {
}

action poste_lang_menu() {
  $c := colors→blue;
  $c1 := colors→green→darken(0.3);
  if not (data→init_done or data→lSpr_set→count = data→languageNames→count) then {
    code→create_lang_Menues(data→rSpr_set, 228, $c);
    code→create_lang_Menues(data→lSpr_set, 0, $c1);
  }
  data→board→post_to_wall;
  meta private;
}
var rSpr_set : Sprite_Set {
}

event swipe_sprite_in_lSpr_set(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  foreach sprite1 in data→lSpr_set where true do {
    $sprite1→set_speed_y($delta_y);
  }
  meta private;
}

event gameloop() {
  data→board→evolve;
  data→lang_source := code→highlight_sprite(data→lSpr_set);
  data→lang_destination := code→highlight_sprite(data→rSpr_set);
  code→stop_scrolling(data→lSpr_set);
  code→stop_scrolling(data→rSpr_set);
  if data→lSpr_set→is_invalid or data→lSpr_set→count = 0 then {
    skip;
  }
  else {
    code→stop_scrolling(data→lSpr_set);
  }
  if data→rSpr_set→is_invalid or data→rSpr_set→count = 0 then {
    skip;
  }
  else {
    code→stop_scrolling(data→rSpr_set);
  }
  data→board→update_on_wall;
  meta private;
}

event tap_sprite_in_lSpr_set(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  code→tap_sprite_in_spset($sprite, data→lSpr_set);
  $key := data→languageNames→keys→at($index_in_set);
  data→lang_source := $key;
  meta private;
}

event swipe_sprite_in_rSpr_set(sprite: Sprite, index_in_set: Number, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  foreach sprite1 in data→rSpr_set where true do {
    $sprite1→set_speed_y($delta_y);
  }
  meta private;
}

event tap_sprite_in_rSpr_set(sprite: Sprite, index_in_set: Number, x: Number, y: Number) {
  code→tap_sprite_in_spset($sprite, data→rSpr_set);
  $key := data→languageNames→keys→at($index_in_set);
  data→lang_destination := $key;
  meta private;
}
var h05 : Number {
}

action create_board() {
  if(data→board→is_invalid or not data→init_done) then {
    data→board := media→create_board(640);
    data→h05 := data→board→height / 2;
    data→board→set_friction(0.01);
  }
  if data→lSpr_set→is_invalid or data→rSpr_set→is_invalid or not data→init_done then {
    data→lSpr_set := data→board→create_sprite_set;
    data→rSpr_set := data→board→create_sprite_set;
  }
  meta private;
}

action highlight_sprite(sprite_set: Sprite_Set) returns lang: String {
  $idx := 0;
  $vy := 0;
  $deltaY := 0;
  $lang := languages→current_language;
  foreach sprite1 in $sprite_set where true do {
    if $sprite1→y < data→h05 + 43 and $sprite1→y > data→h05 - 43 then {
      $sprite1→set_opacity(1);
      $idx := $sprite_set→index_of($sprite1);
      $vy := $sprite1→speed_y;
      $lang := data→languageNames→keys→at($idx);
      if $vy = 0 then {
        $deltaY := data→board→height / 2 - $sprite1→y;
      }
    }
    else {
      $sprite1→set_opacity(0.65);
    }
  }
  foreach sprite in $sprite_set where true do {
    $sprite→move(0, $deltaY);
  }
  meta private;
}

action reset() {
  data→init_done := false;
  code→main;
}

action stop_scrolling(sprite_set: Sprite_Set) {
  $vy := math→abs($sprite_set→at(0)→speed_y);
  $slow := $vy < 20;
  if $sprite_set→at(0)→y ≥ data→h05 or $sprite_set→at(37)→y ≤ data→h05 or $slow then {
    foreach sprite1 in $sprite_set where true do {
      $sprite1→set_speed_y(0);
    }
  }
  meta private;
}

action tap_sprite_in_spset(sprite: Sprite, sprite_set: Sprite_Set) {
  $sprite→set_opacity(1);
  $delta := data→board→height / 2 - $sprite→y;
  foreach sprite1 in $sprite_set where true do {
    $sprite1→set_speed(0, 0);
    $sprite1→move(0, $delta);
  }
  meta private;
}
var lang_source : String {
}
var lang_destination : String {
}
var init_done : Boolean {
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("back") then {
    wall→pop_page;
  }
  if $item→icon→equals("transport.ff") then {
    code→new_page_translate_from_to;
  }
  if $item→text→equals("auto") then {
    wall→push_new_page;
    data→page_translate := wall→current_page;
    data→lang_destination := languages→current_language;
    data→lang_source := "";
    wall→set_subtitle("from auto discovered language to " ∥ data→languageNames→at(data→lang_destination));
    code→translate_from_to;
  }
  meta private;
}
var page_main : Page {
}

action translate_from_to() {
  wall→set_reversed(true);
  wall→set_title("translate");
  wall→add_button("close", "back");
  $source := "";
  while $source→equals("") do {
    $source := wall→ask_string("Enter phrase to translate.");
    if $source→equals("") then {
      wall→clear;
      "Sorry, problems recognizing input." →post_to_wall;
    }
    else {
      $source→copy_to_clipboard;
    }
  }
  $translation := languages→translate(data→lang_source, data→lang_destination, $source);
  wall→clear;
  if data→lang_source→equals("") then {
    data→lang_source := languages→detect_language($source);
  }
  $langName := data→languageNames→at(data→lang_source);
  $langName→post_to_wall;
  $tb_source := wall→create_text_box($source, 32);
  $speak := code→valid_for_speak(data→lang_source);
  records→TextBox_decorator→at($tb_source)→valid_for_speak→set($speak);
  records→TextBox_decorator→at($tb_source)→language→set(data→lang_source);
  $tb_source→set_background(colors→sepia);
  $tb_source→post_to_wall;
  "" →post_to_wall;
  $langName := data→languageNames→at(data→lang_destination);
  "" →post_to_wall;
  $langName→post_to_wall;
  $tb_translation := wall→create_text_box($translation, 32);
  $translation→copy_to_clipboard;
  $speak := code→valid_for_speak(data→lang_destination);
  records→TextBox_decorator→at($tb_translation)→valid_for_speak→set($speak);
  records→TextBox_decorator→at($tb_translation)→language→set(data→lang_destination);
  $tb_translation→set_background(colors→accent);
  $tb_translation→post_to_wall;
  meta private;
}

action valid_for_speak(lang: String) returns speak: Boolean {
  $speak := false;
  // Bing languaged
  // supported fff
  $valid_for_speak := "de,en,fi,fr,it,nl,no,ru,sv" →split(",");
  foreach s in $valid_for_speak where $s→equals($lang) do {
    $speak := true;
  }
  meta private;
}
table thing {
  type = "Decorator";
  keys {
    target: TextBox
  }
  fields {
    language: String_field
    valid_for_speak: Boolean_field
  }
}
var page_translate : Page {
}

event tap_wall_TextBox(item: TextBox) {
  if wall→current_page→equals(data→page_translate) then {
    $speak := records→TextBox_decorator→at($item)→valid_for_speak→get;
    $language := records→TextBox_decorator→at($item)→language→get;
    $s := $item→text;
    if $speak then {
      $sound := languages→speak($language, $s);
      $x := $sound→duration;
      $sound→play;
      time→sleep($x);
    }
  }
  meta private;
}
var isLightTheme : Boolean {
}

action new_page_translate_from_to() {
  wall→push_new_page;
  data→page_translate := wall→current_page;
  wall→set_subtitle("from " ∥ data→languageNames→at(data→lang_source) ∥ " to " ∥ data→languageNames→at(data→lang_destination));
  code→translate_from_to;
  meta private;
}

