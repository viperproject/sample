package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.abstractdomain.Expression


//TODO: Implement equals, toString, hashCode

abstract sealed class Annotation(val classe : Type, val exp : Expression)

case class Invariant(c : Type, e : Expression) extends Annotation(c, e)

case class Predicate(c : Type, val predName : String, e : Expression) extends Annotation(c, e)

case class Precondition(c : Type, val methodName : String, e : Expression) extends Annotation(c, e)

case class Postcondition(c : Type, val methodName : String, e : Expression) extends Annotation(c, e)