
action edit (p:Number) returns  {

  $ok := (false);

  box->edit("textline","",$changehandler);
  where changehandler (text:String) returns () {
    $ok := (true);
  }

  helpers->yield();

  if ($ok) then {
    box->set_foreground(colors->black);
  }

}