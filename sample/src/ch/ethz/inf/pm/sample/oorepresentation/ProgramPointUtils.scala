package ch.ethz.inf.pm.sample.oorepresentation

object ProgramPointUtils {
  implicit val programPointOrdering = new Ordering[ProgramPoint] {
    def compare(p1: ProgramPoint, p2: ProgramPoint): Int = p1.toString.compare(p2.toString)
  }

  /**
   * Returns a program point uniquely identifying a single statement as used in
   * the block semantics (!). The computation finds the leftmost involved program
   * point.
   *
   * @param s A statement as used in the block semantics
   * @return The leftmost involved program point
   */
  def identifyingPP(s: Statement): ProgramPoint = s match {
    case Assignment(pp, l, r) => (pp :: identifyingPP(l) :: identifyingPP(r) :: Nil).min
    case MethodCall(pp, m, _, p, _) => (pp :: identifyingPP(m) :: p.map(identifyingPP)).min
    case VariableDeclaration(pp, v, _, r) =>
      List(Some(pp), Some(identifyingPP(v)), r.map(identifyingPP)).flatten.min
    case FieldAccess(pp, obj, _, _) => List(pp, identifyingPP(obj)).min
    case _ => s.getPC()
  }
}
