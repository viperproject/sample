override def lessEqual(p: Partitioning[D]): Boolean = p match {
	case Top() => true
	case Bottom() => value.lessEqual(value.bottom) 
	case Node(_, _) => value.lessEqual(p.lubState)
	case Leaf(v) => value.lessEqual(v)
}
