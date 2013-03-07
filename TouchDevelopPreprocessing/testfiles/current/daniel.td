action foreachNum() {
  $numbers := collections→create_number_collection;
  $numbers→add(1);
  $numbers→add(2);
  foreach num in $numbers do {
    $numbers→add(3);
    $num→post_to_wall;
  }
}
