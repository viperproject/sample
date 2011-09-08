override def lub(p: Partitioning[D]): Partitioning[D] = p match {
	case Top() => Top()
	case Bottom() => this
	case Node(d, cs) => if (directive.compatible(d)) {
		Node(directive, for ((c1, c2) <- children.zip(cs)) 
				yield lub(c1, c2))
	} else {
		(directive, d) match {
			case (PartitionWhileComputing(_, _), _) => 
				Node(directive, children.patch(1, List(p.lub(p, children(1))), 1))
			case (_, PartitionWhileComputing(_, _)) => 
				Node(d, cs.patch(1, List(this.lub(this, cs(1))), 1))
			case _ => Top()
		}
	}
	case Leaf(v) => directive match {
		case PartitionWhileComputing(_, n) => 
			Node(directive, children.patch(1, List(p.lub(p, children(1))), 1))
		case _ => Node(directive, children.map(lub(_, p)))
	}
}
