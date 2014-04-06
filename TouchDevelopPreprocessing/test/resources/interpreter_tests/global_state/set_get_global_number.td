action main() returns  {
  data->counter := 0;
  code->inc();
  code->inc();
  code->inc();
  contract->assert(data->counter = 3, "must hold");
}

action inc() returns  {
  data->counter := data->counter + 1;
}

var counter : Number {
}