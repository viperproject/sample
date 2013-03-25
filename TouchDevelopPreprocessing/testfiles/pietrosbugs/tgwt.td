meta version "v2.2,nothing";
meta name "Windows Phone News";
meta icon "fourcolumn";
meta color "#ff007fff";
// Новости Windows Phone, обзоры смартфонов. Прошивки, программы и игры.
//
// В новой версии 1.2:
// • Новое название
// • Добавлен фон в скрипте
// • Новая плитка на рабочем столе
//
// ------------------
// Windows Phone News 1.2
// DeveloperRU
meta platform "current";

action main2() {
  data→main_tile→set_back_icon(art→tile_background);
  data→main_tile→set_icon(art→tile_background);
}
var tile_background : Picture {
  is\_resource = true;
  url = "http://w7phone.ru/wp-content/uploads/2012/09/large_pc_tile.png";
}

action main() {
  wall→set_background(art→background);
  wall→prompt("Здесь вы можете найти самые свежие новости о Windows Phone с сайта W7Phone.ru. Чтобы увидеть самые последние новости пролистайте список до конца. Для продолжения нажмите Ok.");
  wall→clear;
  "\n\n© 2013 W7Phone.ru. Все права защищены. При публикации материалов сайта обязательно указывать ссылку на источник. Все права принадлежат их законным владельцам." →post_to_wall;
  $msgs := web→feed(web→download("http://w7phone.ru/feed/"));
  code→set_show_all_at_once(true);
  code→set_messages($msgs);
}

action browse_current_message() {
  if not (data→current_message→is_invalid) and not (data→current_message→link→is_invalid) then {
    web→browse(data→current_message→link);
  }
}

action go_to_index(index: Number) returns found: Boolean {
  if $index ≥ 0 and $index < data→messages→count then {
    data→current_index := $index;
    code→set_current_message(data→messages→at($index));
    $found := true;
  }
  else {
    $found := false;
  }
}

action next() returns found: Boolean {
  $found := code→go_to_index(data→current_index + 1);
}

action previous() returns found: Boolean {
  $found := code→go_to_index(data→current_index - 1);
}

action set_current_message(message: Message) {
  data→current_message := $message;
  data→current_index := data→messages→index_of($message, 0);
  if $message→is_invalid then {
    skip;
  }
  else {
    if $message→is_invalid then {
      skip;
    }
    else {
      if not data→show_all_at_once then {
        wall→set_background(colors→white);
        wall→clear;
        wall→clear_buttons;
        $message→message→post_to_wall;
        if not $message→picture_link→is_invalid then {
          web→download_picture($message→picture_link)→post_to_wall;
        }
        wall→add_button("minus", "Previous")→post_to_wall;
        wall→add_button("folder", "Browse")→post_to_wall;
        wall→add_button("add", "Next")→post_to_wall;
      }
    }
  }
}

action set_messages(msgs: Message_Collection) {
  data→messages := $msgs;
  if data→messages→is_invalid then {
    skip;
  }
  else {
    if data→messages→count > 0 then {
      code→set_current_message(data→messages→at(0));
      if data→show_all_at_once then {
        data→messages→post_to_wall;
      }
    }
  }
}

action set_show_all_at_once(show: Boolean) {
  data→show_all_at_once := $show;
}

action go() {
  skip;
  meta private;
}
var current_index : Number {
}
var show_all_at_once : Boolean {
}
var messages : Message_Collection {
}
var s : String {
}
var current_message : Message {
}
var main_tile : Tile {
  readonly = true;
}
var background : Color {
  is\_resource = true;
  url = "#ff009dc9";
}

