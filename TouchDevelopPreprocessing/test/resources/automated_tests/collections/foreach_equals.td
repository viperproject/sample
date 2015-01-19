meta version "v2.2,js,ctx";
meta name "messages";
meta platform "current";

action main() {
  $msg := social→create_message("test");
  $msgs := collections→create_message_collection;
  $msgs→add($msg);
  foreach msg2 in $msgs
  where $msg2→message→equals("test")
  do {
    $msg2→post_to_wall;
    skip;
  }
}