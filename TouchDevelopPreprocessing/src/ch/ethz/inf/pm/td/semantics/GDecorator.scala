package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TypeList
import ch.ethz.inf.pm.td.parser.TypeName

case class GDecorator(typeName:TypeName, keyTypeName:TypeName,  valueType:AAny) extends AIndex {

  lazy val keyType:AAny = TypeList.toTouchType(keyTypeName)

}
