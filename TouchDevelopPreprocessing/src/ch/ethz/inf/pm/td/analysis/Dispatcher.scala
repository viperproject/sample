package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.td.compiler.{TouchType, CFGGenerator, TouchCompiler}
import RichExpressionImplicits._
import ch.ethz.inf.pm.td.semantics.AAny


/**
 * Implements user-defined libraries
 *
 * User: Lucas
 * Date: 17.02.13
 * Time: 18:54
 */
class Dispatcher() extends NativeMethodSemantics {

  /**
   * Backward semantics are empty for all native function for now
   */
  def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                  parameters : List[ExpressionSet], typeparameters : List[Type],
                                                  returnedtype : Type, pp : ProgramPoint, state : S, oldPreState: S) : Option[S] = None

  /**
   * Delegates forward semantics to concrete classes.
   *
   * Checks if the object or any other
   */
  def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                 parameters : List[ExpressionSet], typeparameters : List[Type],
                                                 returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = {

    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

    if (CFGGenerator.isLibraryIdent(thisExpr.getType().name)) {
      compiler.getMethodWithClassDefinition(operator,thisExpr.getType(),parameters map (_.getType())) match {
        case Some(mdecl: MethodDeclaration) =>

          // We may access a library function
          Some(MethodSummaries.collect(pp, mdecl, state, parameters))

        case _ =>

          // Curiously, this is also used to access global data from libraries.
          if(parameters.isEmpty && returnedtype.toString != "Nothing") {
            // We are just guessing that this may be a global variable
            Some(state.setExpression(ExpressionSet(VariableIdentifier(CFGGenerator.globalReferenceIdent(operator))(returnedtype, pp))))
          } else if (parameters.size == 1 && returnedtype.toString == "Nothing" && operator.startsWith("set ")) {
            // And we have also automatically generated setters. What the...
            val List(newVal) = parameters
            val variableName = operator.replace("set ","")
            val variableType = newVal.getType()
            val variableExpr = ExpressionSet(VariableIdentifier(CFGGenerator.globalReferenceIdent(variableName))(variableType, pp))
            Some(RichNativeSemantics.Assign[S](variableExpr, newVal)(state,pp))
          } else {
            Reporter.reportImprecision("Could not find method "+operator,pp)
            None
          }

      }
    } else {
      Some(thisExpr.getType().asInstanceOf[AAny].forwardSemantics[S](thisExpr,operator,parameters,returnedtype.asInstanceOf[TouchType])(pp,state))
    }

  }

}
