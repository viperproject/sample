package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.TouchField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Key / Value pair for collections
 */
case class GEntry(key:TypeName,value:TypeName) extends AAny {

  override def typeName: TypeName = TypeName("Entry",List(key,value))

  lazy val field_key = TouchField("*key", key)

  lazy val field_value = TouchField("*value", value)

  override def possibleFields = super.possibleFields ++ List(field_key,field_value)

}
