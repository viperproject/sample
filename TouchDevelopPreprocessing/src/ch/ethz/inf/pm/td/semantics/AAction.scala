package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
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
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "run" -> member_run
  )

  def actionArguments: List[ApiParam]

  /** Stores a string representing the handler in the code. When an action is defined in the code, the
    * corresponding action is created with a unique name (e.g. program point based) and this object is
    * returned with the handlerName field set to the name of the created action. If this field
    * is top, and run is executed, we have to go to top, since we do not know what is executed */
  lazy val field_handlerName = ApiField("*handlername",TString)

  override def possibleFields = super.possibleFields + field_handlerName

}
