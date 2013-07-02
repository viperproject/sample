action main () {
    $loc := locations->create_location(0, 5);
    contract->assert($loc->longitude = 5,"");
}