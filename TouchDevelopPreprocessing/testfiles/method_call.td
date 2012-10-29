var start : Number {}

action main() { 
	data->start := 0 + 1 - 1;
	code->test(); 
}

action test() { 
	data->start := 1;
}