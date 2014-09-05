package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.{SystemParameters, oorepresentation}
import ch.ethz.inf.pm.td.analysis.TouchField
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.sample.abstractdomain.Identifier


trait TouchType extends Named with Type {

  def name:String = typeName.toString
  def typeName:TypeName

  def isSingleton: Boolean
  def isImmutable: Boolean

  def isBottom = this == BottomTouchType
  def isTop = this == TopTouchType

  def factory() = top()
  def top() = TopTouchType
  def bottom() = BottomTouchType

  def lub(other: oorepresentation.Type): oorepresentation.Type = {
    if (other == null) return this
    val other_ = other.asInstanceOf[TouchType]
    if (isTop || other_.isTop) return top()
    if (isBottom) return other_
    if (other_.isBottom) return this
    if (!equals(other_)) top()
    else this
  }

  def glb(other: oorepresentation.Type): oorepresentation.Type = {
    if (other == null) return this
    val other_ = other.asInstanceOf[TouchType]
    if (isBottom || other_.isBottom) return bottom()
    if (isTop) return other_
    if (other_.isTop) return this
    if (!equals(other_)) bottom()
    else this
  }

  def widening(other: Type) = lub(other)

  def lessEqual(other: Type) = other == this || this.isBottom || other == top()

  def isBottomExcluding(types: Set[Type]) = isBottom || types.contains(this)

  def isObject = !isNumericalType && !isStringType
  def isBooleanType = name == "Boolean"
  def isNumericalType = (name == "Number") || (name == "Boolean")
  def isFloatingPointType = name == "Number" || name == "Boolean" // TODO: Booleans should not be floating points
  def isStringType = name == "String"
  def isStatic = isSingleton
  def possibleTouchFields: Set[TouchField] = possibleFields map (_.asInstanceOf[TouchField])
  def arrayElementsType = None

}

case object TopTouchType extends TouchType {
  val typeName = TypeName("Top")
  override def isTop: Boolean = true

  // these don't really make sense here
  val isSingleton = false
  val isImmutable = false
}

case object BottomTouchType extends TouchType {
  val typeName = TypeName("Bottom")
  override def isBottom: Boolean = true

  // these don't really make sense here
  val isSingleton = false
  val isImmutable = false

}

//case class DefaultTouchType(
//  typeName: TypeName,
//  isSingleton: Boolean = false,
//  isImmutable: Boolean = false,
//  fields: List[Identifier] = Nil) extends TouchType {
//
//  override def possibleFields: Set[Identifier] = super.possibleFields ++ fields.toSet
//}
