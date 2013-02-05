action main () {      
    $ls := collections->create_location_collection;
    $loc2 := locations->create_location(0, 0);
    $loc := locations->create_location(0, 5);
    $ls->add($loc);
    $ls->add($loc2);
    $ls->remove($loc);
    $expected0or5 := $ls->at(0)->longitude;
}