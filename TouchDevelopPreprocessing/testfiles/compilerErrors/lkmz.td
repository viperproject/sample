meta version "v2.2,nothing";
meta name "Austin Chronicle RSS";
meta icon "Business";
meta color "#ff85BB65";
//
meta platform "current";

action main() {
  wall→set_foreground(colors→from_rgb(0, .8, .8));
  wall→set_background(colors→from_rgb(.1, .1, .1));
  wall→set_title("Austin Chronicle RSS Feed");
  $msgs := web→feed(web→download("http://www.austinchronicle.com/gyrobase/rss/issue.xml"));
  code→set_show_all_at_once(true);
  code→set_messages($msgs);
}
var messages : Message_Collection {
}
var current_message : Message {
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
var current_index : Number {
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
var show_all_at_once : Boolean {
}

action set_show_all_at_once(show: Boolean) {
  data→show_all_at_once := $show;
}

action browse_current_message() {
  if not (data→current_message→is_invalid) and not (data→current_message→link→is_invalid) then {
    web→browse(data→current_message→link);
  }
}

event tap_wall_Page_Button(button: Page_Button) {
  if $button→text→equals("Next") then {
    code→next;
  }
  else {
    if $button→text→equals("Previous") then {
      code→previous;
    }
    else {
      if $button→text→equals("Browse") then {
        code→browse_current_message;
      }
    }
  }
  meta private;
}
table thing {
  type = "Object";
}
var c : Color {
  is\_resource = true;
  url = "#ff5dbfbb";
}
var c2 : Color {
  is\_resource = true;
  url = "#ff3faaaf";
}

