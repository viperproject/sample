def whileExample(x: Int): Int = {
	var y = 0
	var m = 0
	var c = 0
	var i = 0

	while (i < (x+2)/2 && i < 3) {
		 i = i + 1
	}

	if (i == 0) { c = 0; m = 0 }
	else if (i == 1) { c = 0; m = 1 }
	else if (i == 2) { c = 4; m = -1 }
	else if (i == 3) { c = 0; m = 0 }
	else return 0

	y = c + m*x
	y
}
