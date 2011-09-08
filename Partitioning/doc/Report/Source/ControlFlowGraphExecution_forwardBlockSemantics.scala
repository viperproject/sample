private def forwardBlockSemantics(s: S, b: List[Statement]): List[S] = b match {
	case x :: xs => 
		val sp = s.before(identifyingPP(x))
		sp :: forwardBlockSemantics(x.forwardSemantics(sp), xs)
	}
	case Nil => s :: Nil
}
