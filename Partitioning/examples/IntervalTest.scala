class IntervalTest {

	def ifTest(x: Int): Int = {
		var sign = 0
		if (x < 0) {		// Position: 5, 21 ?
			sign = -1
		} else {
			sign = 1
		}
		val y = x / sign
		y
	}

	def ndTest(x: Int, y: Int): Int = {
		if (x < 0) {
			y + 2
		} else {
			y - 2
		}
	}

	def loopTest(x: Int): Int = {
		var i = 0

		while (i < x) {
			i = i + 1
		}

		i
	}

	def nestedIfTest(x: Int): Int = {
		var i = 0
		var sign = 0
		var y = 0

		while (i < x) {
			if (x < 0) {		// Position 38, 29
				sign = -1
			} else {
				sign = 1
			}
			y = x / sign
			i = i + 1
		}

		y
	}

}

