class PartitionValue {

	def partitionValueExample(x: Int): Int  = {
		var y = 0
		var c = 0
		var m = 0

		if (x < -1) c = -1
		if (x > 1) c = -1

		if (x < -1) m = -1
		if (x > 1) m = 1

		y = c + m*x
		y
	}

}