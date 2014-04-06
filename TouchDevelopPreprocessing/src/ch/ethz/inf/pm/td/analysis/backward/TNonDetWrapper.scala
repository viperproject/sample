package ch.ethz.inf.pm.td.analysis.backward

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.semantics.TouchField
import ch.ethz.inf.pm.sample.abstractdomain.Identifier

case class TNonDetWrapper(wrappedType: TouchType) extends TouchType {

  val name: String = "NonDetWrapper"

  val valueField = new TouchField("value", wrappedType.name)

  def innerType: TouchType = valueField.typ

  def possibleFields: Set[Identifier] = Set(valueField)

  def isSingleton: Boolean = false

  def isImmutable: Boolean = false
}
