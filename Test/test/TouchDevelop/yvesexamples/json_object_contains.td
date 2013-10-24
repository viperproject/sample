action main() {

    if web->is_connected() then {
        $js := web->download_json("http://www.ethz.ch");
        if not $js->is_invalid() then {
            if $js->keys()->contains("name") then {
                $n := $js->field("name");
                $n->post_to_wall();
            }
            $e := $js->field("name");
            $e->post_to_wall();
        }
    }
}