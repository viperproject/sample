package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.parser.TypeName

case class GDecorator(typeName:TypeName, keyType:AAny, valueType:AAny) extends AIndex {

}
