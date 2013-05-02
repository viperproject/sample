package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.property._


sealed trait ExpectedOutput {
  def cover(o : Output) : Boolean;
}

case class WarningPP(val ppIdent:String) extends ExpectedOutput {
  override def cover(o : Output) : Boolean = o match {
    case WarningProgramPoint(pp, message) => return pp.toString.equals(ppIdent)
    case _ => return false;
  }

  override def toString() : String = "warningPP(" +ppIdent+ ")"
}

case class ValidatedPP(val ppIdent:String) extends ExpectedOutput {
  override def cover(o : Output) : Boolean = o match {
    case ValidatedProgramPoint(pp, message) => return pp.toString.equals(ppIdent)
    case _ => return false;
  }

  override def toString() : String = "validatedPP(" +ppIdent+ ")"
}

case class WarningMethod(val classe : String, val method : String) extends ExpectedOutput {
  override def cover(o : Output) : Boolean = o match {
    case ch.ethz.inf.pm.sample.property.WarningMethod(classe, method, message) => return classe.getName().equals(this.classe) && method.equals(this.method)
    case _ => return false;
  }

  override def toString() : String = "warningMethod(" +classe+ ", " +method+ ")"
}

case class ValidatedMethod(val classe : String, val method : String) extends ExpectedOutput  {
  override def cover(o : Output) : Boolean = o match {
    case ch.ethz.inf.pm.sample.property.ValidatedMethod(classe, method, message) => return classe.getName().equals(this.classe) && method.equals(this.method)
    case _ => return false;
  }

  override def toString() : String = "validatedMethod(" +classe+ ", " +method+ ")"
}

case class InferredContract(val contract : Contract) extends ExpectedOutput  {
  override def cover(o : Output) : Boolean = o match {
    case ch.ethz.inf.pm.sample.property.InferredContract(c) => return contract.cover(c);
    case _ => return false;
  }

  override def toString() : String = "inferredContract("+contract.toString+")"
}


sealed abstract class Contract(val exp : String) {
  def cover(o : ch.ethz.inf.pm.sample.oorepresentation.Annotation) : Boolean = o.exp.toString().equals(exp)
}

case class Invariant(val classe : String, e : String) extends Contract(e) {
  override def cover(o : ch.ethz.inf.pm.sample.oorepresentation.Annotation) : Boolean = o match {
    case ch.ethz.inf.pm.sample.oorepresentation.Invariant(c, e) => super.cover(o) && c.equals(classe)
    case _ => false;
  }

  override def toString() : String = "invariant(" + classe + ", \"" + exp + "\")"
}

case class Predicate(val classe : String, val name : String, e : String) extends Contract(e) {
  override def cover(o : ch.ethz.inf.pm.sample.oorepresentation.Annotation) : Boolean = o match {
    case ch.ethz.inf.pm.sample.oorepresentation.Predicate(c, predName, e) => super.cover(o) && predName.equals(name) && c.equals(classe)
    case _ => false;
  }

  override def toString() : String = "predicate(" + classe + "," + name + ", \"" + exp + "\")"
}

case class PreCondition(val classe : String, val method : String, e : String) extends Contract(e) {
  override def cover(o : ch.ethz.inf.pm.sample.oorepresentation.Annotation) : Boolean = o match {
    case ch.ethz.inf.pm.sample.oorepresentation.PreCondition(c, methodName, e) => super.cover(o) && methodName.equals(method) && c.equals(classe)
    case _ => false;
  }

  override def toString() : String = "precondition(" + classe + "," + method + ", \"" + exp + "\")"
}

case class PostCondition(val classe : String, val method : String, e : String) extends Contract(e) {
  override def cover(o : ch.ethz.inf.pm.sample.oorepresentation.Annotation) : Boolean = o match {
    case ch.ethz.inf.pm.sample.oorepresentation.PostCondition(c, methodName, e) => super.cover(o) && methodName.equals(method) && c.equals(classe)
    case _ => false;
  }

  override def toString() : String = "postcondition(" + classe + "," + method + ", \"" + exp + "\")"
}

case class LoopInvariant(val ppIdent:String, e : String) extends Contract(e) {
  override def cover(o : ch.ethz.inf.pm.sample.oorepresentation.Annotation) : Boolean = o match {
    case ch.ethz.inf.pm.sample.oorepresentation.LoopInvariant(pp, e) => super.cover(o) && pp.toString.equals(ppIdent)
    case _ => false;
  }

  override def toString() : String = "loopinvariant(" + ppIdent +", \"" + exp + "\")"
}
