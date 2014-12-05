package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.parser.TypeName

case class GDecorator(valueType:AAny, decoratedType:AAny) extends AIndex {

  override def typeName = TypeName("Decorator",List(decoratedType.typeName))

}
