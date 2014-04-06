action main() returns  {
  code->otherAction(true);
}

action otherAction(param: Boolean) returns  {
  //:: ExpectedOutput(assert.failed)
  contract->assert($param->not, "does not hold");
}