package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import scala.Error
import scala.Some
import ch.ethz.inf.pm.td.analysis.MethodSummaries
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.sample.{SystemParameters, Reporter}

/**
 * Implements user-defined libraries
 *
 * User: Lucas
 * Date: 17.02.13
 * Time: 18:54
 */
class Libraries() extends NativeMethodSemantics {

  /**
   * Backward semantics are empty for all native function for now
   */
  def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                  parameters : List[ExpressionSet], typeparameters : List[Type],
                                                  returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = None

  /**
   * Delegates forward semantics to concrete classes.
   *
   * Checks if the object or any other
   */
  def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                 parameters : List[ExpressionSet], typeparameters : List[Type],
                                                 returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = {

    SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethod(operator,parameters map (_.getType())) match {
      case Some((clazz,methodDef)) =>
        Some(MethodSummaries.collect(pp,clazz,methodDef,state,parameters))
      case _ =>
        Reporter.hasImprecision("Could not find this method "+operator,pp)
        None
    }

  }

}
