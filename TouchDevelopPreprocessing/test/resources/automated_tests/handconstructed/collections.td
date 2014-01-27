action main () {      
    $ls := collections->create_location_collection;
    $loc2 := locations->create_location(0, 0);
    $loc := locations->create_location(0, 5);
    $ls->add($loc);
    $ls->add($loc2);
    $ls->remove($loc);
    contract->assert($ls->at(0)->longitude = 0,"");
    contract->assert($ls->at(1)->longitude = 5,"");
    contract->assert(($ls->at(1)->longitude = 0)->not,"");
    contract->assert(($ls->at(0)->longitude = 5)->not,"");
}