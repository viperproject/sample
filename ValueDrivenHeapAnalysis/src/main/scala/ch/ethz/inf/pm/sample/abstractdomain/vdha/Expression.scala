/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.{Expression, Identifier}

case class ValueHeapIdentifier(obj: HeapVertex, field: Identifier)
  extends Identifier.HeapIdentifier {

  def pp = field.pp

  def typ = field.typ

  def getName = s"${obj.name}.$field"

  def getField = Some(field.getName)

  def representsSingleVariable = obj.isInstanceOf[DefiniteHeapVertex]
}

/** Represents a value field of the object pointed from or to by the edge
  * which this edge-local identifier belongs to.
  *
  * An empty access path means that the edge-local identifier
  * refers to a value field of the source object of the corresponding edge.
  *
  * Analogously, a non-empty access path means that the edge-local identifier
  * refers to a value field of the target object of the corresponding edge.
  *
  * Note that edges going out of a local variable vertex are treated in exactly
  * the same way. Such edges have the field `None` and so will the edge-local
  * identifier.
  */
case class EdgeLocalIdentifier(accPath: List[Option[String]], field: Identifier)
  extends Identifier {

  require(!typ.isObject, "EdgeLocalIdentifier should represent value information.")
  require(accPath.size <= 1, "For now, we allow at most single step look-ahead.")

  import ValueDrivenHeapStateConstants._

  def getName: String = {
    val fullPath = List(edgeLocalIdentifier) ++ accPath.flatten ++ List(field)
    fullPath.mkString(".")
  }

  def typ = field.typ

  def pp = field.pp

  def getField = Some(field.getName)

  /** An edge-local identifier always represents a field of a single object. */
  def representsSingleVariable = true

  /** Whether the edge-local identifier refers to a field of the source. */
  def isForSource = accPath.isEmpty

  /** Whether the edge-local identifier refers to a field of the target. */
  def isForTarget = accPath.nonEmpty
}

object EdgeLocalIdentifier {
  /** Creates an edge-local identifier with an empty access path from a field identifier. */
  def apply(field: Identifier): EdgeLocalIdentifier =
    EdgeLocalIdentifier(List.empty, field)
}

case class VertexExpression(typ: Type, vertex: Vertex)(val pp: ProgramPoint) extends Expression {
  def ids = throw new Exception("should never be called")

  override def toString: String = vertex.name

  def transform(f: (Expression) => Expression): Expression = f(this)

  def contains(f: (Expression => Boolean)): Boolean = f(this)

}