package ch.ethz.inf.pm.sample.oorepresentation

/**
 * This class represents a generic annotation
 *
 * @param exp The expression (represented by a string) of the annotation
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
abstract sealed class Annotation(val exp : String) {
  override def hashCode() : Int = 1;

  override def equals(o : Any) : Boolean = o match {
    case x: Annotation => return x.exp.equals(exp)
    case _ => false
  }
  def getMessage()=exp;

}

/**
 * This class represents a class invariant
 *
 * @param classe The name of the class
 * @param e The expression
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class Invariant(val classe : String, e : String) extends Annotation(e) {
  override def equals(o : Any) : Boolean = o match {
    case x: Invariant => return x.classe.equals(classe) && super.equals(x);
    case _ => false
  }

    override def toString() = "Invariant of class "+classe+": "+exp;
}

/**
 * This class represents an abstract predicate
 *
 * @param classe The name of the class
 * @param predName The name of the predicate
 * @param e The expression
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class Predicate(val classe : String, val predName : String, e : String) extends Annotation(e) {
  override def equals(o : Any) : Boolean = o match {
    case x: Predicate => return x.classe.equals(classe) && x.predName.equals(predName) && super.equals(x);
    case _ => false
  }

    override def toString() = "Predicate "+predName+" in class "+classe+": "+exp;
}

/**
 * This class represents a precondition
 *
 * @param classe The name of the class
 * @param methodName The name of the method
 * @param e The expression
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class PreCondition(val classe : String, val methodName : String, e : String) extends Annotation(e) {
    override def equals(o : Any) : Boolean = o match {
      case x: PreCondition => return x.classe.equals(classe) && x.methodName.equals(methodName) && super.equals(x);
      case _ => false
    }

    override def toString() = "Precondition in class "+classe+" of method "+methodName+": "+exp;
}

/**
 * This class represents a postcondition
 *
 * @param classe The name of the class
 * @param methodName The name of the method
 * @param e The expression
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class PostCondition(val classe : String, val methodName : String, e : String) extends Annotation(e) {
    override def equals(o : Any) : Boolean = o match {
      case x: PostCondition => return x.classe.equals(classe) && x.methodName.equals(methodName) && super.equals(x);
      case _ => false
    }

    override def toString() = "Postcondition in class "+classe+" of method "+methodName+": "+exp;
}

/**
 * This class represents a loop invariant
 *
 * @param pp The program point of the loop invariant
 * @param e The expression
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
case class LoopInvariant(val pp : ProgramPoint, e : String) extends Annotation(e) {
    override def equals(o : Any) : Boolean = o match {
      case x: LoopInvariant => return x.pp.equals(pp) && super.equals(x);
      case _ => false
    }

    override def toString() = "Loop invariant "+pp.getDescription+": "+exp;
}