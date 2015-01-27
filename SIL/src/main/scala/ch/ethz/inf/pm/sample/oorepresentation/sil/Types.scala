package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.Identifier
import ch.ethz.inf.pm.sample.oorepresentation.Type

/** Abstract base class for all types in the SIL type hierarchy. */
abstract class AbstractType(val name: String) extends Type {
  override def toString: String = name

  /** Apron does not consider this flag.
    * It represents all numerical values as floating-point values.
    */
  def isFloatingPointType: Boolean = false

  def isBooleanType: Boolean = this == BoolType

  def isObject: Boolean = !isNumericalType

  def isStringType: Boolean = false

  def isStatic: Boolean = false

  def arrayElementsType: Option[Type] = None

  def isBottomExcluding(types: Set[Type]): Boolean = false

  def possibleFields = Set.empty

  def factory() = top()

  def top() = TopType

  def bottom() = BottomType

  def widening(other: Type) = lub(other)

  def lessEqual(r: Type): Boolean = r == this || bottom == this || r == top

  def lub(other: Type): Type = (this, other) match {
    case (null, _) => other
    case (_, null) => this
    case (_, TopType) | (TopType, _) => top()
    case (BottomType, _) => other
    case (_, BottomType) => this
    case _ if this == other => this
    case _ => top()
  }

  def glb(other: Type) = (this, other) match {
    case (null, _) => other
    case (_, null) => this
    case (BottomType, _) | (_, BottomType) => bottom()
    case (TopType, _) => other
    case (_, TopType) => this
    case _ if this == other => this
    case _ => top()
  }

  def isBottom = this == BottomType
  def isTop = this == TopType
}

case object TopType extends AbstractType("Top") {
  def isNumericalType = false
}

case object BottomType extends AbstractType("Bottom") {
  def isNumericalType = false
}

case object BoolType extends AbstractType("Bool") {
  def isNumericalType = true
}

case object IntType extends AbstractType("Int") {
  def isNumericalType = true
}

case object PredType extends AbstractType("Pred") {
  // Do not create vertices and edges in abstract heap graphs
  def isNumericalType = true
}

case class RefType(var fields: Set[Identifier] = Set.empty) extends AbstractType("Ref") {
  override def possibleFields = fields

  def isNumericalType = false
}