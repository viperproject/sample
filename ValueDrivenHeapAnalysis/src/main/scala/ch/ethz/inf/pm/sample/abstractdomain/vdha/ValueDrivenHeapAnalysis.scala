package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, ExpressionSet, Analysis}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.oorepresentation.NativeMethodSemantics
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.{BooleanNativeMethodSemantics, IntegerNativeMethodSemantics, ObjectNativeMethodSemantics}
import ch.ethz.inf.pm.sample.property.Property
import apron.Polka

class ValueDrivenHeapAnalysis extends Analysis {

  def getLabel(): String = "Value Driven Heap analysis"

  def parameters(): List[(String, Any)] = Nil

  def setParameter(label: String, value: Any) {
    //    label match {
    //      case "Domain" => value match {
    //        case "Interval" => domain = new Box()
    //        case "PPL" => domain = new Polka(false) //new PplPoly(false); FIXIT: Change back to PPL
    //        case "Octagons" => domain = new Octagon()
    //        case "Polka" => domain = new Polka(false)
    //        case "Linear equalities" => domain = new PolkaEq()
    //      }
    //    }
  }

  def reset() {}

  def getInitialState(): ValueDrivenHeapState[ApronInterface] = {
    val manager = new Polka(false)
    // val manager = new Octagon()
    // val manager = new Box()
    val generalValState = new ApronInterface(None, manager, env =  Set.empty[Identifier]).top()
    ValueDrivenHeapState(new HeapGraph[ApronInterface](), generalValState, ExpressionSet())
  }

  def getProperties(): List[Property] = List(
    //    new ShowGraphProperty().asInstanceOf[Property]
    //    new SingleStatementProperty(DivisionByZero),
    //    new SingleStatementProperty(new LowerBoundedValue("y", 0)),
    //    new SingleStatementProperty(new BoundedValue("y", -4, 4))
  )

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = List(ObjectNativeMethodSemantics, IntegerNativeMethodSemantics, BooleanNativeMethodSemantics)

}