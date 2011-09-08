override def lessEqual(p: Partitioning[D]): Boolean = p match {
	case Top() => true
	case Bottom() => children.forall(_.lessEqual(Bottom()))
	case Node(d, cs) => directive.compatible(d) && 
		children.indices.forall(i => children(i).lessEqual(cs(i)))
	case Leaf(v) => false
}
