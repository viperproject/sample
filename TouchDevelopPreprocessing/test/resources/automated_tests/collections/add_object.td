action main(){
    $msg := social->create_message("test");
    $c := collections->create_message_collection();
    $c->add($msg);

}