action main () {      
    $ls := collections->create_location_collection;
    $loc2 := locations->create_location(0, 0);
    $loc := locations->create_location(0, 5);
    $ls->add($loc);
    $ls->add($loc2);
    $ls->remove($loc)->post_to_wall;
    $ls->at(0)->longitude->post_to_wall;
}