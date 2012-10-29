import ch.ethz.inf.pm.sample.abstractdomain.Expression
import ch.ethz.inf.pm.sample.oorepresentation.{Variable, ProgramPoint}

class ForAllExpression(val pp : ProgramPoint, val v : Variable, val exp : Expression) extends Expression(pp) {
  override def hashCode() = exp.hashCode()
  override def getType() = new SILType("Boolean")
  override def equals(a : Any) = a match {
    case x : ForAllExpression => pp.equals(x.pp) && v.equals(x.v) && exp.equals(x.exp)
    case _ => false;
  }
  override def toString() = "For all "+v.toString()+": "+exp.toString
}

class ExistExpression(val pp : ProgramPoint, val v : Variable, val exp : Expression) extends Expression(pp)  {
  override def hashCode() = exp.hashCode()
  override def getType() = new SILType("Boolean")
  override def equals(a : Any) = a match {
    case x : ExistExpression => pp.equals(x.pp) && v.equals(x.v) && exp.equals(x.exp)
    case _ => false;
  }
  override def toString() = "Exists "+v.toString()+": "+exp.toString
}