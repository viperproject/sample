package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.parser.TypeName

case class GIndex(valueType:AAny) extends AIndex {

  def typeName = TypeName("Index",List(valueType.typeName))



}
