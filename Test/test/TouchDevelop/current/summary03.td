action main () {      
    $ls := collections->create_location_collection;
    $loc := locations->create_location(0, 5);
    $ls->add($loc);
    contract->assert($ls->at(0)->longitude = 5,"");
}