import ch.ethz.inf.pm.sample.abstractdomain.Expression
import ch.ethz.inf.pm.sample.oorepresentation.{Variable, ProgramPoint}
import silAST.domains.DomainPredicate

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

class OldExpression(val pp : ProgramPoint, val exp : Expression) extends Expression(pp)  {
  override def hashCode() = exp.hashCode()
  override def getType() = exp.getType()
  override def equals(a : Any) = a match {
    case x : OldExpression => pp.equals(x.pp) && exp.equals(x.exp)
    case _ => false;
  }
  override def toString() = "old("+exp.toString+")"
}

class FieldPermissionExpression(val pp : ProgramPoint, val location : Expression, val permission : Expression) extends Expression(pp)  {
  override def hashCode() = location.hashCode()
  override def getType() = new SILType("Boolean")
  override def equals(a : Any) = a match {
    case x : FieldPermissionExpression => pp.equals(x.pp) && location.equals(x.location) && permission.equals(x.permission)
    case _ => false;
  }
  override def toString() = "acc("+location.toString+", "+permission.toString+")"
}

class PredicatePermissionExpression(val pp : ProgramPoint, val predicate : Expression, val permission : Expression) extends Expression(pp)  {
  override def hashCode() = predicate.hashCode()
  override def getType() = new SILType("Boolean")
  override def equals(a : Any) = a match {
    case x : PredicatePermissionExpression => pp.equals(x.pp) && predicate.equals(x.predicate) && permission.equals(x.permission)
    case _ => false;
  }
  override def toString() = "acc("+predicate.toString+", "+permission.toString+")"
}

class UnfoldingExpression(val pp : ProgramPoint, val predicate : PredicatePermissionExpression, val permission : Expression) extends Expression(pp)  {
  override def hashCode() = predicate.hashCode()
  override def getType() = new SILType("Boolean")
  override def equals(a : Any) = a match {
    case x : UnfoldingExpression => pp.equals(x.pp) && predicate.equals(x.predicate) && permission.equals(x.permission)
    case _ => false;
  }
  override def toString() = "unfolding "+predicate.toString+" in "+permission.toString+")"
}

class DomainPredicateExpression(val pp : ProgramPoint, val predicate : DomainPredicate, val arguments : List[Expression]) extends Expression(pp)  {
  override def hashCode() = predicate.hashCode()
  override def getType() = new SILType("Boolean")
  override def equals(a : Any) = a match {
    case x : DomainPredicateExpression => pp.equals(x.pp) && predicate.equals(x.predicate) && arguments.equals(x.arguments)
    case _ => false;
  }
  override def toString() = predicate.toString+"("+arguments.toString+")"
}