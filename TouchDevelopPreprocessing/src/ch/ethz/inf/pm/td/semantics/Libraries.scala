package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{EmptyScopeIdentifier, VariableIdentifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import scala.{None, Some}
import ch.ethz.inf.pm.td.analysis.MethodSummaries
import ch.ethz.inf.pm.td.compiler.{CFGGenerator, TouchCompiler}
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

    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

    if (CFGGenerator.isLibraryIdent(thisExpr.getType().getName())) {
      compiler.getMethodWithClassDefinition(operator,thisExpr.getType(),parameters map (_.getType())) match {
        case Some((clazz,methodDef)) =>

          // We may access a library function
          Some(MethodSummaries.collect(pp,clazz,methodDef,state,parameters))

        case _ =>

          // Curiously, this is also used to access global data from libraries.
          if(parameters.isEmpty && returnedtype.toString != "Nothing") {
            // We are just guessing that this may be a global variable
            Some(state.setExpression(new ExpressionSet(returnedtype).add(VariableIdentifier(CFGGenerator.globalReferenceIdent(operator),returnedtype,pp,EmptyScopeIdentifier()))))
          } else if (parameters.size == 1 && returnedtype.toString == "Nothing" && operator.startsWith("set ")) {
            // And we have also automatically generated setters. What the...
            val List(newVal) = parameters
            val variableName = operator.replace("set ","")
            val variableType = newVal.getType()
            val variableExpr = new ExpressionSet(variableType).add(VariableIdentifier(CFGGenerator.globalReferenceIdent(variableName),variableType,pp,EmptyScopeIdentifier()))
            Some(RichNativeSemantics.Assign[S](RichNativeSemantics.toRichExpression(variableExpr),RichNativeSemantics.toRichExpression(newVal))(state,pp))
          } else {
            Reporter.reportImprecision("Could not find method "+operator,pp)
            None
          }

      }
    } else { None }

  }

}
