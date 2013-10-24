action main(){
        if web->is_connected() then {
            $js := web->download_json("http://www.ethz.ch");
            $text := $js->field("text");
            $text2 := $text->string("text");
            $text->post_to_wall();
        }
}