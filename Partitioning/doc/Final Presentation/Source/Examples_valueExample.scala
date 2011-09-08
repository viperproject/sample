def valueExample(x: Int): Int = {
	val c = Array(-1, 0, -1)
	val m = Array(-1, 0, 1)

	var i = 0

	if (x < -1) i = 0
	else if (x > 1) i = 2
	else i = 1

	val y = c(i) + m(i)*x
	return y
}
