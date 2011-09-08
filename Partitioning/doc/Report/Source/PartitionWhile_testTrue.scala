override def testTrue(p: Partitioning[D]): Partitioning[D] = p match {
	case Node(d, c) => if (compatible(d)) {
		val ci = if (c(0) != Bottom()) c(0) else c.last
		Node(PartitionWhileComputing(pp, n), 
				ci :: Bottom[D]() :: c.tail.take(n))
	} else {
		Node(d, c.map(testTrue(_)))
	}
	case _ => p
}
