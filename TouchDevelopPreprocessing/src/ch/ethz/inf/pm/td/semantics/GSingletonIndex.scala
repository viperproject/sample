package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.parser.TypeName

case class GSingletonIndex(indexMemberType:AAny) extends AAny {

  lazy val field_singleton = new ApiField("singleton", indexMemberType)

  def typeName = TypeName("Index",List(indexMemberType.typeName))

  override def possibleFields = super.possibleFields + field_singleton

}
