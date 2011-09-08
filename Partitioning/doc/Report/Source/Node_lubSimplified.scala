override def lub(p: Partitioning[D]): Partitioning[D] = p match {
	case Top() => Top()
	case Bottom() => this
	case Node(d, cs) => if (directive.compatible(d)) {
			Node(directive, for ((c1, c2) <- children.zip(cs)) 
					yield lub(c1, c2))
		} else {
			case _ => Top()
		}
	case Leaf(v) =>  Node(directive, children.map(lub(_, p)))
}
