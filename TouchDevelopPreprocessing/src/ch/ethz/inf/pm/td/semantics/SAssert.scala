package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{State, ExpressionSet}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * Specifies the abstract semantics of assertions
 *
 * Assertions are not part of the original TouchDevelop language, but are useful for debugging
 *
 * @author Lucas Brutschy
 */

object SAssert {

  val typName = "assert"
  val typ = new TouchType(typName, isSingleton = true)
//
//  val methods = Map(
//    "is_true" -> new LibraryMethod("is_true") {
//      def semantics[S <: State[S]](this0: ExpressionSet, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
//        Error[S]((parameters.head).not(), "Assertion "+parameters.head+" does not hold!")
//        Skip
//      }
//    }
//
//  )

}

//abstract class LibraryMethod(name:String) {
//
//  def getName:String = name
//  def semantics[S <: State[S]](this0:ExpressionSet, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S
//  def mayRead:List[TouchField] = Nil
//  def mayWrite:List[TouchField] = Nil
//
//}

class SAssert extends AAny {

  def getTyp = SAssert.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Break if the given assertion does not hold */
    case "is_true" =>
      Error[S]((parameters.head).not(), "Assertion "+parameters.head+" does not hold!")
      Skip

    /** Break if the given assertion does hold */
    case "is_false" =>
      Error[S](parameters.head, "Assertion not( "+parameters.head+" ) does not hold!")
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}