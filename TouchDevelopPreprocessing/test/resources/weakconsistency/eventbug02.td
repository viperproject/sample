
action edit (p:Number) returns  {

  data->ok := (false);

  box->edit("textline","",$changehandler);
  where changehandler (text:String) returns () {
    data->ok := (true);
  }

  helpers->yield();

  if (data->ok) then {
    box->set_foreground(colors->black);
  }

}

var ok : Boolean {}