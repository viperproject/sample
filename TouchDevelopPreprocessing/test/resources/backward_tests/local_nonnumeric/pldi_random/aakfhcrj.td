//:: ExpectedOutput(backward.counterexample.found)
action main() {
  $v := collections→create_number_collection;
  $v→add(123);
  $v→add(456);
  $v→add( - 789);
  $v→post_to_wall;
  $dt := time→today;
  $dt→post_to_wall;
  $p := senses→acceleration_quick;
  //:: ExpectedOutput(assert.failed)
  $p→post_to_wall;
}
