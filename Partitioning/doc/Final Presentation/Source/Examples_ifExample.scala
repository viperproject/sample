def ifExample(x: Int): Int = {
	var sign = 0
	if (x < 0) {
		sign = -1
	} else {
		sign = 1
	}
	var y = x / sign
	y
}
