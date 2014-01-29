action main(){
    if web->is_connected() then {
            $js := web->download_json("http://www.ethz.ch");
    }
}