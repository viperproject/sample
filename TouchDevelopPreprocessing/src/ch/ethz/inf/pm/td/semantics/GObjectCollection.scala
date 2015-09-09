package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{Modifier, ProgramPoint}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

case class GObjectCollection(objectTyp:AAny,modifiers:Set[Modifier]) extends AMutableLinearCollection {

  def typeName = TypeName("Collection", List(objectTyp.typeName))
  def keyType = TNumber
  def valueType = objectTyp

}
