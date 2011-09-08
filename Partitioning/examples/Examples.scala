class Examples {

	def ifExample(x: Int): Int = {
		var sign = 0
		if (x < 0) {											// PartitionIf((5,21))
			sign = -1
		} else {
			sign = 1
		}
		val y = x / sign
		y																	// Merge((11,17), (5,21))
	}

	def valueExample(x: Int): Int = {
		var y = 0													// PartitionValue((15,21), x, (-inf,-2), (-1,1), (2,inf))
		var c = 0
		var m = 0

		if (x < -1) c = -1								// Pseudo array access
		else if (x > 1) c = -1

		if (x < -1) m = -1								// Pseudo array access
		else if (x > 1) m = 1

		y = c + m*x
		y																	// Merge((28,17), (15,21))
	}

	def whileExample(x: Int): Int = {
		var y = 0
		var m = 0
		var c = 0
		var i = 0

		while (i < (x+2)/2 && i < 3) {		// PartitionWhile((35,24), 3)
			 i = i + 1
		}

		if (i == 0) { c = 0; m = 0 }			// Pseudo array access
		else if (i == 1) { c = 0; m = 1 }
		else if (i == 2) { c = 4; m = -1 }
		else if (i == 3) { c = 0; m = 0 }
		else return 0

		y = c + m*x
		y																	// Merge()
	}

}
