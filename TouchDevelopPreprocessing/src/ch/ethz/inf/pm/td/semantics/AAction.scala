package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, MethodSummaries, ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler._
import RichNativeSemantics._

/**
 * General definition for Actions (closure types)
 *
 * @author Lucas Brutschy
 */
trait AAction extends AAny {

  /** Never used: Run the inline action. */
  def member_run = ApiMember(
    name = "run",
    paramTypes = actionArguments,
    thisType = ApiParam(this),
    returnType = actionReturnValue,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S) = {

        val context = SystemParameters.analysisUnitContext
        val classType = context.clazzType
        val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

        def defaultBehavior() = {
          if (TouchAnalysisParameters.get.defaultToUnsound) {
            Top[S](actionReturnValue)
          } else {
            // call all handlers
            Lattice.bigLub(compiler.allMethods map { x =>
              if (CFGGenerator.isHandlerIdent(x.name.toString)) {
                MethodSummaries.collect(pp, x, state, parameters)
              } else {
                state.bottom()
              }
            })
          }
        }


        EvalConstant[S](Field[S](this0,field_handlerName)) match {
          case SetDomain.Default.Bottom() => state.bottom()
          case SetDomain.Default.Top() =>
            Reporter.reportImprecision("Handler name is top", pp)
            defaultBehavior()
          case SetDomain.Default.Inner(xs) =>
            Lattice.bigLub(
              xs map { x =>
                compiler.getMethodWithClassDefinition(x.constant, classType, parameters map (_.getType())) match {
                  case Some(mdecl) =>
                    MethodSummaries.collect(pp, mdecl, state, parameters)
                  case _ =>
                    Reporter.reportImprecision("Invalid handler name " + x.constant, pp)
                    defaultBehavior()
                }
              }
            )
        }

      }
    }
  )

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "run" -> member_run
  )

  def actionArguments: List[ApiParam]
  def actionReturnValue: AAny = TNothing

  /** Stores a string representing the handler in the code. When an action is defined in the code, the
    * corresponding action is created with a unique name (e.g. program point based) and this object is
    * returned with the handlerName field set to the name of the created action. If this field
    * is top, and run is executed, we have to go to top, since we do not know what is executed */
  lazy val field_handlerName = ApiField("*handlername",TString)

  override def possibleFields = super.possibleFields + field_handlerName

}
