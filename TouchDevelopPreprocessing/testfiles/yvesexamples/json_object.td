action main() {
    if web->is_connected() then {
        $x := web->download_json("http://www.ethz.ch");

        foreach r in $x where true do {
            $s := $r->field("name");
        }
    }
}