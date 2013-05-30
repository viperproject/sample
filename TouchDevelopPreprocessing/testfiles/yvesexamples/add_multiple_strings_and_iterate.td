var c: String_Collection {

}

action init(){
    $item_count := wall->ask_number("How many items?");
    data->c := collections->create_string_collection();
    for 0 ≤ i < $item_count do {
        data->c→add("item " ∥ ($i + 1));
    }
}

action print(){
    foreach e in data->c where true do {
        $e->post_to_wall();
    }
}


action main(){
    code->init();
    code->print();
}