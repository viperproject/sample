action main () {      
    $ls := collections->create_location_collection;
    $loc2 := locations->create_location(0, 0);
    $loc := locations->create_location(0, 5);
    $ls->add($loc);
    $ls->add($loc2);
    $ls->remove($loc);
    assert->is_true($ls->at(0)->longitude = 0);
    assert->is_true($ls->at(1)->longitude = 5);
    assert->is_false($ls->at(1)->longitude = 0);
    assert->is_false($ls->at(0)->longitude = 5);
}