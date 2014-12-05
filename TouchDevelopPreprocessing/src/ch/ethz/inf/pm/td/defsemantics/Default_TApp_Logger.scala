
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of App Logger
 *
 * A custom logger
 *
 * @author Lucas Brutschy
 */

trait Default_TApp_Logger extends AAny {

  lazy val typeName = TypeName("App Logger")
          
  /** Never used: Logs a debug message */
  def member_debug = ApiMember(
    name = "debug",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Logs an error message */
  def member_error = ApiMember(
    name = "error",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Logs an informational message */
  def member_info = ApiMember(
    name = "info",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Logs a new message with optional metadata. The level follows the syslog convention. */
  def member_log = ApiMember(
    name = "log",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Logs a warning message */
  def member_warning = ApiMember(
    name = "warning",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "debug" -> member_debug,
    "error" -> member_error,
    "info" -> member_info,
    "log" -> member_log,
    "warning" -> member_warning
  )
            

}
          
