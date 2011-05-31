package ch.ethz.inf.pm.sample.oorepresentation

abstract sealed class Annotation(val exp : String) {
  override def hashCode() : Int = 1;

  override def equals(o : Any) : Boolean = o match {
    case x: Annotation => return x.exp.equals(exp)
    case _ => false
  }

}

case class Invariant(val classe : String, e : String) extends Annotation(e) {
  override def equals(o : Any) : Boolean = o match {
    case x: Invariant => return x.classe.equals(classe) && super.equals(x);
    case _ => false
  }

    override def toString() = "Invariant of class "+classe+": "+exp;
}

case class Predicate(val classe : String, val predName : String, e : String) extends Annotation(e) {
  override def equals(o : Any) : Boolean = o match {
    case x: Predicate => return x.classe.equals(classe) && x.predName.equals(predName) && super.equals(x);
    case _ => false
  }

    override def toString() = "Predicate "+predName+" in class "+classe+": "+exp;
}

case class PreCondition(val classe : String, val methodName : String, e : String) extends Annotation(e) {
    override def equals(o : Any) : Boolean = o match {
      case x: PreCondition => return x.classe.equals(classe) && x.methodName.equals(methodName) && super.equals(x);
      case _ => false
    }

    override def toString() = "Precondition in class "+classe+" of method "+methodName+": "+exp;
}

case class PostCondition(val classe : String, val methodName : String, e : String) extends Annotation(e) {
    override def equals(o : Any) : Boolean = o match {
      case x: PostCondition => return x.classe.equals(classe) && x.methodName.equals(methodName) && super.equals(x);
      case _ => false
    }

    override def toString() = "Postcondition in class "+classe+" of method "+methodName+": "+exp;
}

case class LoopInvariant(val pp : ProgramPoint, e : String) extends Annotation(e) {
    override def equals(o : Any) : Boolean = o match {
      case x: LoopInvariant => return x.pp.equals(pp) && super.equals(x);
      case _ => false
    }

    override def toString() = "Loop invariant at line "+pp.getLine()+" and "+pp.getColumn()+": "+exp;
}