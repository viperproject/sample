class PartitionWhile {

	def partitionWhileTest(x: Int) = {
		var i = 0
		while (i < x) {
			i = i + 1
		}
		i
	}

	def partitionWhileNestedTest(x: Int, y: Int) {
		var i = 0
		while (i < x) {
			i = i + 1
			var j = 0
			while (j < y) {
				j = j + 1
			}
		}
	}

	def partitionWhileExample(x: Int): Int = {
		var y = 0
		var m = 0
		var c = 0
		var i = 0

		while (i < (x+2)/2 && i < 3) i = i + 1

		if (i == 0) { c = 0; m = 0 }		// pseudo-array access
		if (i == 1) { c = 0; m = 1 }
		if (i == 2) { c = 4; m = -1 }
		if (i == 3) { c = 0; m = 0 }

		y = c + m*x
		y
	}

}