/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, TypeMap}

object DummyTypeMap extends TypeMap {

  override val Int: Type = DummyIntegerType

  override val Float: Type = DummyFloatType

  override val String: Type = DummyStringType

  override val Boolean: Type = DummyBooleanType

  override val Bottom: Type = DummyBottomType

  override val Top: Type = DummyTopType

}

/**
  *
  * A dummy type with no proper hierarchy for testing.
  *
  * @author Pietro Ferrara, Lucas Brutschy
  *
  */
trait DummyType extends Type {

  def factory(): DummyType = this

  def top(): DummyType = DummyTopType

  def bottom(): DummyType = DummyBottomType

  def lub(other: Type): Type = {
    if (other == this) this
    else if (other.isBottom) this
    else if (this.isBottom) other
    else top()
  }

  def glb(other: Type): Type = {
    if (other == this) this
    else if (other.isTop) this
    else if (this.isTop) other
    else bottom()
  }

  def widening(other: Type): Type =
    this.lub(other)

  def lessEqual(other: Type): Boolean = {
    if (other == this) true
    else if (other.isTop) true
    else if (this.isBottom) true
    else false
  }

  override def isFloatingPointType = false

  override def isBooleanType = false

  override def isNumericalType = false

  override def isStringType = false

  override def isObject = false

  override def isStatic = false

  override def isBottom = false

  override def isTop = false

  override def possibleFields: Set[Identifier] = Set.empty

}

/** A dummy object type with no proper hierarchy for testing. */
trait DummyObjectType extends DummyType {

  override def isObject = true

}

/** A dummy numerical type with no proper hierarchy for testing. */
case object DummyIntegerType extends DummyType {

  def name = "Int"

  override def isNumericalType = true

}

/** A dummy numerical type with no proper hierarchy for testing. */
case object DummyFloatType extends DummyType {

  def name = "Float"

  override def isNumericalType = true

  override def isFloatingPointType = true

}

case object DummyTopType extends DummyType {

  def name = "Top"

  override def isTop = true

}

case object DummyBottomType extends DummyType {

  def name = "Bottom"

  override def isBottom = true

}

/** A dummy boolean type with no proper hierarchy for testing. */
case object DummyBooleanType extends DummyType {

  def name = "Bool"

  override def isNumericalType = true

  override def isBooleanType = true

}

/** A dummy boolean type with no proper hierarchy for testing. */
case object DummyStringType extends DummyType {

  def name = "String"

  override def isStringType = true

}