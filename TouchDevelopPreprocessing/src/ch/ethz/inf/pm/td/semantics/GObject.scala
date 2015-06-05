package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

case class GObject(typeName:TypeName, fields:List[ApiField]) extends AAny {

  lazy val member_clear_fields = ApiMember(
    name = "clear fields",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  override def possibleFields = super.possibleFields ++ fields

  override lazy val declarations = super.declarations ++ mkGetterSetters(fields) ++ Map("clear fields" -> member_clear_fields)

}
