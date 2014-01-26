
action main() {

  data->everywhere := media->create_board(640);

  data->thickness := 4;
  $a := 4;
  $b := 6;
  $c := code->func1($a + 2);
  $d := code->func1($b + 2);

  $local_object_to_stay_in_main := media->create_board(640);

  contract->assert($a = 4,"");
  contract->assert($b = 6,"");
  contract->assert($c = 4,"");
  contract->assert($d = 6,"");
  contract->assert(data->thickness = 6,"");

}

action func1(a: Number) returns b:Number {

    $b := $a - 2;
    data->thickness := data->thickness + 1;

    $local_object_to_stay_in_func1 := media->create_board(640);

    meta private;

}

var thickness : Number {
}

var everywhere : Board {
}
