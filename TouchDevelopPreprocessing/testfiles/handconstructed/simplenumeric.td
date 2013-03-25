
action main() {
  $a := 5;
  $b := 6;
  $c := $a / $b;
  skip;
  if $c > 0 then {
    $a := 3;
  }
}
