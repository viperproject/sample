override def lub(p: Partitioning[D]): Partitioning[D] = p match {
	case Top() => Top()
	case Bottom() => this
	case Node(_, _) => p.lub(p, this)
	case Leaf(v) => Leaf(value.lub(value, v))
}
