meta version "v2.2,nothing";
meta name "pictionary";
meta icon "shuffle";
meta color "#ffff0038";
// shows random pictograms of animals, fruits, vegetables and other things. Select language, tap pic to translate. New version with FAQ - mode. Color bug fix.
meta platform "current";

action main() {
  data→counter := 0;
  data→solved := 0;
  data→mode := 0;
  $s := languages→current_language;
  data→language := $s;
  data→i\u002Dmode := true;
  code→init_animal_map;
  code→init_food_map;
  code→init_things_map;
  code→init_all_things_map;
  code→create_langNameMap;
  code→poste_lang_menu;
  code→post_mode_menu;
}

action init_animal_map() {
  if data→animals_map→is_invalid or true then {
    data→animals_map := collections→create_string_map;
    data→animals_map→set_at("🐜", "ant");
    data→animals_map→set_at("🐝", "bee");
    data→animals_map→set_at("🐗", "boar");
    data→animals_map→set_at("🐂", "bull");
    data→animals_map→set_at("🐇", "bunny");
    data→animals_map→set_at("🐫", "camel");
    data→animals_map→set_at("🐈", "cat");
    data→animals_map→set_at("🐓", "cock");
    data→animals_map→set_at("🐄", "cow");
    data→animals_map→set_at("🐣", "chick");
    data→animals_map→set_at("🐕", "dog");
    data→animals_map→set_at("🐘", "elephant");
    data→animals_map→set_at("🐠", "fish");
    data→animals_map→set_at("🐬", "dolphin");
    data→animals_map→set_at("🐪", "dromedary");
    data→animals_map→set_at("🐸", "frog");
    data→animals_map→set_at("🐨", "koala");
    data→animals_map→set_at("🐔", "chicken");
    data→animals_map→set_at("🐞", "ladybug");
    data→animals_map→set_at("🐒", "monkey");
    data→animals_map→set_at("🐀", "mouse");
    data→animals_map→set_at("🐙", "octopus");
    data→animals_map→set_at("🐧", "penguin");
    data→animals_map→set_at("🐖", "pig");
    data→animals_map→set_at("🐦", "pigeon");
    data→animals_map→set_at("🐩", "poodle");
    data→animals_map→set_at("🐢", "turtle");
    data→animals_map→set_at("🐑", "sheep");
    data→animals_map→set_at("🐐", "goat");
    data→animals_map→set_at("🐏", "aries");
    data→animals_map→set_at("🐎", "horse");
    data→animals_map→set_at("🐍", "serpent");
    data→animals_map→set_at("🐌", "snail");
    data→animals_map→set_at("🐋", "whale");
  }
  meta private;
}
var animals_map : String_Map {
}
var board : Board {
}
var sprite_pic : Sprite {
}

action init_board() {
  data→board := media→create_board(460);
  data→board→set_background(colors→chrome);
  $pic1 := media→create_picture(400, 400);
  data→sprite_pic := data→board→create_picture($pic1);
  meta private;
}

action create_rnd_sprite() {
  if not data→i\u002Dmode then {
    data→counter := data→counter + 1;
  }
  data→sprite_pic→hide;
  data→sprite_pic→delete;
  records→Sprite_decorator→clear;
  $s := "";
  if data→mode = 0 then {
    $s := code→get_random_string(data→animals_map);
  }
  else {
    if data→mode = 1 then {
      $s := code→get_random_string(data→food_map);
    }
    else {
      if data→mode = 2 then {
        $s := code→get_random_string(data→things_map);
      }
      else {
        if data→mode = 3 then {
          $s := code→get_random_string(data→all_things_map);
        }
      }
    }
  }
  $pic := media→create_picture(400, 400);
  $c1 := colors→random;
  $x := $c1→brightness;
  if colors→is_light_theme and $c1→brightness > 0.8 then {
    $c1 := $c1→darken(0.4);
  }
  else {
    if not colors→is_light_theme and $c1→brightness < 0.4 then {
      $c1 := $c1→lighten(0.4);
    }
  }
  $pic→fill_rect(0, 0, 400, 400, 0, $c1);
  $pic→draw_text(10, 10, $s, 380, 0, colors→chrome);
  $c := colors→linear_gradient($c1, colors→background, 0);
  $pic→draw_rect(0, 0, 400, 400, 0, $c, 3);
  data→sprite_pic := data→board→create_picture($pic);
  $s1 := "";
  if data→mode = 0 then {
    $s1 := data→animals_map→at($s);
  }
  else {
    if data→mode = 1 then {
      $s1 := data→food_map→at($s);
    }
    else {
      if data→mode = 2 then {
        $s1 := data→things_map→at($s);
      }
      else {
        if data→mode = 3 then {
          $s1 := data→all_things_map→at($s);
        }
      }
    }
  }
  $t := records→Sprite_decorator→at(data→sprite_pic);
  $t→name→set($s1);
  $t→is_pic→set(true);
  data→sprite_pic→show;
  meta private;
}

event tap_sprite\u003A_sprite_pic(sprite: Sprite, x: Number, y: Number) {
  $is_pic := records→Sprite_decorator→at($sprite)→is_pic→get;
  if $is_pic then {
    $equal := false;
    if data→i\u002Dmode then {
      code→create_translation;
    }
    else {
      wall→push_new_page;
      wall→set_reversed(true);
      $pic := $sprite→picture→clone;
      $pic→post_to_wall;
      $pic→resize(100, - 1);
      $s1 := wall→ask_string("Solution?");
      $s1 := $s1→to_lower_case;
      $s := records→Sprite_decorator→at(data→sprite_pic)→name→get;
      $s2 := languages→translate("en", data→language, $s);
      $s2 := $s2→to_lower_case;
      $equal := $s2→contains($s1);
      wall→clear;
      wall→pop_page;
      if $equal then {
        data→solved := data→solved + 1;
        if data→mode = 0 then {
          code→decrement_strg_map(data→animals_map, $s);
        }
        else {
          if data→mode = 1 then {
            code→decrement_strg_map(data→food_map, $s);
          }
          else {
            if data→mode = 2 then {
              code→decrement_strg_map(data→things_map, $s);
            }
            else {
              if data→mode = 3 then {
                code→decrement_strg_map(data→all_things_map, $s);
              }
            }
          }
        }
      }
      $check := "✔ ";
      if $equal then {
        $check := "✔ ";
      }
      else {
        $check := "✘ ";
      }
      ... ;
      wall→set_subtitle($check ∥ "tried: " ∥ data→counter ∥ "  solved: " ∥ data→solved);
      code→create_translation;
      ... ;
    }
  }
  else {
    code→create_rnd_sprite;
  }
  data→board→update_on_wall;
  meta private;
}
table thing {
  type = "Decorator";
  keys {
    target: Sprite
  }
  fields {
    name: String_field
    is_pic: Boolean_field
  }
}
var solve : Boolean {
}

action create_translation() {
  $s := records→Sprite_decorator→at(data→sprite_pic)→name→get;
  $s := $s→to_upper_case;
  $s := languages→translate("en", data→language, $s);
  $x_offset := 200 - $s→count / 2 * 20;
  $pic1 := data→sprite_pic→picture→clone;
  data→sprite_pic→hide;
  data→sprite_pic→delete;
  records→Sprite_decorator→clear;
  $pic := media→create_picture(400, 400);
  $c1 := colors→random;
  $c2 := $pic1→pixel(20, 370)→darken(0.1);
  $c2→make_transparent(0.8);
  if false then {
    $pic→fill_rect(0, 0, 400, 400, 0, $c1);
  }
  else {
    $pic→fill_rect(5, 352, 390, 45, 0, $c2);
  }
  $pic→draw_text($x_offset, 355, $s, 36, 0, colors→background);
  $pic1→blend($pic, 0, 0, 0, 0.95);
  $c := colors→linear_gradient($c1, colors→blue, 0);
  if true then {
    skip;
  }
  data→sprite_pic := data→board→create_picture($pic1);
  $s1 := data→animals_map→at($s);
  $t := records→Sprite_decorator→at(data→sprite_pic);
  $t→is_pic→set(false);
  data→sprite_pic→show;
  data→board→update_on_wall;
  $b := code→valid_for_speak(data→language);
  if $b then {
    $sound := languages→speak(data→language, $s);
    if not $sound→is_invalid then {
      $x := $sound→duration;
      $sound→play;
      time→sleep($x);
    }
  }
  meta private;
}

action poste_lang_menu() {
  records→TextBox_decorator→clear;
  wall→set_reversed(true);
  data→page_lang_menu := wall→current_page;
  $strings := data→languageNames→keys;
  $x := $strings→count;
  $x1 := $x;
  foreach s in $strings where true do {
    $x := $x - 1;
    $c := colors→linear_gradient(colors→magenta, colors→blue, $x / $x1);
    $s1 := data→languageNames→at($s);
    $tb := wall→create_text_box($s1, 40);
    $tb→set_foreground($c);
    records→TextBox_decorator→at($tb)→lang_short_name→set($s);
    $tb→post_to_wall;
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
table thing2 {
  type = "Decorator";
  keys {
    target: TextBox
  }
  fields {
    lang_short_name: String_field
  }
}

event tap_wall_TextBox(item: TextBox) {
  if wall→current_page→equals(data→page_lang_menu) then {
    $t := records→TextBox_decorator→at($item);
    data→language := $t→lang_short_name→get;
    code→post_mode_menu;
  }
  else {
    if wall→current_page→equals(data→page_mode_menu) then {
      if $item→text→equals("ANIMALS") then {
        data→mode := 0;
      }
      else {
        if $item→text→equals("FOODS") then {
          data→mode := 1;
        }
        else {
          if $item→text→equals("VARIOUS OBJECTS") then {
            data→mode := 2;
          }
          else {
            if $item→text→equals("ALL OBJECTS") then {
              data→mode := 3;
            }
            else {
              data→mode := 0;
            }
          }
        }
      }
      code→open_board;
    }
  }
  meta private;
}
var language : String {
}

action open_board() {
  wall→push_new_page;
  wall→add_button("check", "i-mode");
  wall→set_reversed(true);
  $s := data→languageNames→at(data→language);
  wall→set_title($s);
  code→init_board;
  code→create_rnd_sprite;
  data→board→post_to_wall;
  meta private;
}

action valid_for_speak(lang: String) returns speak: Boolean {
  // Bing language speech support for
  // ca,da,de,en,es,fi,fr,it,ja,ko,no,nl,pl,pt,ru,sv,zh-CHS,zh-CHT
  $speak := false;
  $valid_for_speak := "ca,da,de,en,es,fi,fr,it,ja,ko,no,nl,pl,pt,ru,sv,zh-CHS,zh-CHT" →split(",");
  foreach s in $valid_for_speak where $s→equals($lang) do {
    $speak := true;
  }
  meta private;
}

action init_food_map() {
  if data→food_map→is_invalid or true then {
    data→food_map := collections→create_string_map;
    data→food_map→set_at("🍓", "strawberry");
    data→food_map→set_at("🍒", "cherry");
    data→food_map→set_at("🍑", "peach");
    data→food_map→set_at("🍐", "pear");
    data→food_map→set_at("🍎", "Apple");
    data→food_map→set_at("🍍", "pineapple");
    data→food_map→set_at("🍌", "banana");
    data→food_map→set_at("🍋", "lemon");
    data→food_map→set_at("🍉", "melon");
    data→food_map→set_at("🍇", "grapes");
    data→food_map→set_at("🍅", "tomato");
    data→food_map→set_at("🍄", "mushroom");
    data→food_map→set_at("🍰", "torte");
    data→food_map→set_at("🍭", "lolly");
    data→food_map→set_at("🍬", "candy");
    data→food_map→set_at("🍫", "chocolate");
    data→food_map→set_at("🍪", "tortilla");
    data→food_map→set_at("🍩", "donut");
    data→food_map→set_at("🍨", "ice cream");
    data→food_map→set_at("🍟", "fries");
    data→food_map→set_at("🍞", "bread");
    data→food_map→set_at("🍜", "bowl of noodle");
    data→food_map→set_at("🍕", "pizza");
    data→food_map→set_at("🍔", "hamburger");
    data→food_map→set_at("🍚", "bowl of rice");
  }
  meta private;
}
var food_map : String_Map {
}
var page_lang_menu : Page {
}

action post_mode_menu() {
  wall→push_new_page;
  wall→add_button("back", "lang");
  wall→set_reversed(true);
  ... ;
  data→page_mode_menu := wall→current_page;
  $tb := wall→create_text_box("ANIMALS", 40);
  $pic := media→create_picture(60, 60);
  $pic→draw_text(0, 0, "🐓", 55, 0, colors→blue);
  $pic→update_on_wall;
  $tb→set_icon($pic);
  $tb→post_to_wall;
  $tb1 := wall→create_text_box("FOODS", 40);
  $pic := media→create_picture(60, 60);
  $pic→draw_text(0, 0, "🍇", 55, 0, colors→green);
  $tb1→set_icon($pic);
  $pic→update_on_wall;
  $tb1→post_to_wall;
  $tb2 := wall→create_text_box("VARIOUS OBJECTS", 40);
  $pic := media→create_picture(60, 60);
  $pic→draw_text(0, 0, "☁", 55, 0, colors→blue);
  $pic→update_on_wall;
  $tb2→set_icon($pic);
  $tb2→post_to_wall;
  $tb3 := wall→create_text_box("ALL OBJECTS", 40);
  $pic := media→create_picture(60, 60);
  $pic→draw_text(0, 0, "🐈🍎\n👪👪", 25, 0, colors→red);
  $pic→update_on_wall;
  $tb3→set_icon($pic);
  $tb3→post_to_wall;
  meta private;
}
var page_mode_menu : Page {
}
var mode : Number {
}

action init_things_map() {
  if data→things_map→is_invalid or true then {
    data→things_map := collections→create_string_map;
    data→things_map→set_at("🍴", "cutlery");
    data→things_map→set_at("🍵", "coffee cup");
    data→things_map→set_at("🍷", "glass");
    data→things_map→set_at("🎷", "saxophone");
    data→things_map→set_at("🎸", "guitar");
    data→things_map→set_at("🎻", "violin");
    data→things_map→set_at("🎺", "trumpet");
    data→things_map→set_at("✄", "scissors");
    data→things_map→set_at("✈", "airplane");
    data→things_map→set_at("✋", "hand");
    data→things_map→set_at("✎", "pencil");
    data→things_map→set_at("❀", "flower");
    data→things_map→set_at("❤", "heart");
    data→things_map→set_at("⛟", "truck");
    data→things_map→set_at("☂", "umbrella");
    data→things_map→set_at("☁", "cloud");
    data→things_map→set_at("⛵", "sail boat");
    data→things_map→set_at("☎", "telephone");
    data→things_map→set_at("🔨", "hammer");
    data→things_map→set_at("🔪", "knife");
    data→things_map→set_at("📷", "camera");
    data→things_map→set_at("📱", "cell phone");
    data→things_map→set_at("📖", "book");
    data→things_map→set_at("🌏", "globe");
    data→things_map→set_at("🌟", "star");
    data→things_map→set_at("🌴", "palm tree");
    data→things_map→set_at("🌵", "cactus");
    data→things_map→set_at("🌷", "tulip");
    data→things_map→set_at("🎃", "calabash");
    data→things_map→set_at("👀", "eyes");
    data→things_map→set_at("👂", "ear");
    data→things_map→set_at("👃", "nose");
    data→things_map→set_at("👄", "mouth");
    data→things_map→set_at("👅", "tongue");
    data→things_map→set_at("👓", "glasses");
    data→things_map→set_at("🎭", "masks");
    data→things_map→set_at("💏", "kissing");
    data→things_map→set_at("🔔", "chime");
    data→things_map→set_at("🔒", "padlock");
    data→things_map→set_at("🔑", "key");
    data→things_map→set_at("🔎", "magnifier");
    data→things_map→set_at("🔌", "power cort");
    data→things_map→set_at("💡", "bulb");
    data→things_map→set_at("💣", "bomb");
    data→things_map→set_at("👻", "ghost");
    data→things_map→set_at("👼", "angle");
    data→things_map→set_at("👶", "baby");
    data→things_map→set_at("👪", "familie");
    data→things_map→set_at("👕", "shirt");
    data→things_map→set_at("👖", "trousers");
    data→things_map→set_at("👗", "dress");
    data→things_map→set_at("👞", "shoe");
    data→things_map→set_at("👢", "boots");
    data→things_map→set_at("🏰", "castle");
    data→things_map→set_at("🏠", "house");
  }
  meta private;
}
var things_map : String_Map {
}
var all_things_map : String_Map {
}

action init_all_things_map() {
  if data→all_things_map→is_invalid or true then {
    data→all_things_map := collections→create_string_map;
    data→all_things_map→set_many(data→animals_map);
    data→all_things_map→set_many(data→food_map);
    data→all_things_map→set_many(data→things_map);
  }
  meta private;
}

event page_navigated_from() {
  if wall→current_page→equals(data→page_mode_menu) then {
    data→mode := 0;
  }
  meta private;
}

action clear_string_maps() {
  foreach key in data→animals_map→keys where true do {
    data→animals_map→remove($key);
  }
  foreach key1 in data→food_map→keys where true do {
    data→food_map→remove($key1);
  }
  foreach key2 in data→things_map→keys where true do {
    data→things_map→remove($key2);
  }
  foreach key3 in data→all_things_map→keys where true do {
    data→all_things_map→remove($key3);
  }
  data→animals_map := invalid→string_map;
  data→food_map := invalid→string_map;
  data→things_map := invalid→string_map;
  data→all_things_map := invalid→string_map;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→icon→equals("back") then {
    wall→pop_page;
  }
  else {
    data→i\u002Dmode := $item→text→equals("i-mode") and $item→icon→contains("check");
    if data→i\u002Dmode then {
      wall→clear_buttons;
      wall→add_button("back", "lang");
      wall→add_button("questionmark", "? mode");
      data→i\u002Dmode := false;
    }
    else {
      wall→clear_buttons;
      wall→add_button("back", "lang");
      wall→add_button("check", "i-mode");
      data→i\u002Dmode := true;
    }
  }
  meta private;
}
var i\u002Dmode : Boolean {
}

action get_random_string(d1: String_Map) returns s: String {
  $x := $d1→count;
  if $x = 0 then {
    "Congratulations, you solved all items." →post_to_wall;
    time→stop;
  }
  $s := $d1→keys→random;
  meta private;
}

action decrement_strg_map(strg_map: String_Map, s: String) {
  foreach key in $strg_map→keys where $strg_map→at($key)→equals($s) do {
    $strg_map→remove($key);
  }
  meta private;
}
var counter : Number {
}
var solved : Number {
}

