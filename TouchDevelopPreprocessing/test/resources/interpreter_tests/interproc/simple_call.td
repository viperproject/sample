action main() returns  {
  $i := 1;
  code->otherAction($i);
}

action otherAction(param: Number) returns  {
  contract->assert($param = 1, "must hold");
}