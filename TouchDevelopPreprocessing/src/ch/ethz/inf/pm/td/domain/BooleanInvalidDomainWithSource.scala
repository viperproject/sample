/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.BooleanExpressionSimplifier
import ch.ethz.inf.pm.sample.abstractdomain.{AbstractOperator, BinaryBooleanExpression, BinaryNondeterministicExpression, Constant, NegatedBooleanExpression, _}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.td.compiler.TouchException
import ch.ethz.inf.pm.td.domain.ValiditySet._
import ch.ethz.inf.pm.td.semantics.TBoolean


/**
  *
  * Lucas Brutschy
  * Date: 10/18/12
  * Time: 10:50 AM
  *
  */
case class BooleanInvalidDomainWithSource(map: Map[Identifier, ValiditySet] = Map.empty[Identifier, ValiditySet],
    override val isBottom: Boolean = false,
    isTop: Boolean = false)
  extends BoxedDomain[ValiditySet, BooleanInvalidDomainWithSource]
    with BooleanExpressionSimplifier[BooleanInvalidDomainWithSource]
    with InvalidDomain[BooleanInvalidDomainWithSource] {

  def functionalFactory(_value: Map[Identifier, ValiditySet] = Map.empty[Identifier, ValiditySet],
      _isBottom: Boolean = false,
      _isTop: Boolean = false): BooleanInvalidDomainWithSource =
    BooleanInvalidDomainWithSource(_value, _isBottom, _isTop)

  def get(key: Identifier): ValiditySet = map.get(key) match {
    case None => Bottom
    case Some(x) => x
  }

  def getMustExist(key: Identifier): ValiditySet = map.get(key) match {
    case None =>
      throw TouchException("A variable was not created")
    case Some(x) => x
  }

  override def createVariable(variable: Identifier, typ: Type): BooleanInvalidDomainWithSource = {
    if (!map.contains(variable)) this.add(variable, Top)
    else this
  }

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    var result = Map.empty[Identifier, List[String]]
    result = result + ((variable, path ::: variable.toString :: Nil))
    (this.add(variable, Top), result)
  }

  override def setToTop(variable: Identifier): BooleanInvalidDomainWithSource = {
    // Check necessary, otherwise bottomness of state is lost
    if (this.isBottom) return this

    this.add(variable, Top)
  }

  override def removeVariable(variable: Identifier): BooleanInvalidDomainWithSource = {
    // Check necessary, otherwise bottomness of state is lost
    if (this.isBottom) return this

    this.remove(variable)
  }

  override def setArgument(variable: Identifier, expr: Expression): BooleanInvalidDomainWithSource = {
    this.assign(variable, expr)
  }

  override def assign(variable: Identifier, expr: Expression): BooleanInvalidDomainWithSource = {
    val result = eval(expr) match {
      case ValiditySet.Bottom => bottom()
      case res: ValiditySet =>
        if (variable.representsSingleVariable) this.add(variable, res)
        else this.add(variable, getMustExist(variable).lub(res))
    }
    result
  }

  override def backwardAssign(oldPreState: BooleanInvalidDomainWithSource, variable: Identifier, expr: Expression): BooleanInvalidDomainWithSource = {
    val s = assume(BinaryArithmeticExpression(variable, expr, ArithmeticOperator.==, TBoolean))
    s.setToTop(variable)
  }

  /**
    * Evaluation for invalid values. The following rules apply:
    *
    * 1) For every arithmetic operation, the expression may be invalid if one of the two operands may be invalid.
    * The expression must be invalid, if one of the two operands must be invalid.
    * 2) Invalid constant = invalid, all other constants = valid
    *
    */
  private def eval(expr: Expression): ValiditySet = expr match {
    case BinaryArithmeticExpression(left, right, _, typ) => Valid
    case BinaryNondeterministicExpression(left, right, _, typ) => Valid
    case AbstractOperator(left, List(right), Nil, AbstractOperatorIdentifiers.stringConcatenation, _) => Valid
    case InvalidExpression(typ, str, pp) => Invalid(str, pp)
    case ValidExpression(typ, pp) => Valid
    case Constant(_, _, _) => Valid
    case x: BinaryBooleanExpression => Valid
    case x: NegatedBooleanExpression => Valid
    case h: HeapIdentifier => Valid
    case x: Identifier => this.getMustExist(x)
    case x: Expression => Top
  }

  /**
    * We only implement the most necessary part of this (completing this should be quite hard)
    * E ::= x == invalid | x != invalid | E and E | E or E
    *
    * TODO: We would have to convert this to some normal Form
    * TODO: Case x == y, x != y
    */
  override def assumeSimplified(expr: Expression): BooleanInvalidDomainWithSource = {
    val res = expr match {

      case BinaryArithmeticExpression(a: Expression, b: Expression, ArithmeticOperator.==, _) =>

        val left = eval(a)
        val right = eval(b)
        if ((left.mustBeValid && right.mustBeInvalid) || (left.mustBeInvalid && right.mustBeValid))
          return bottom()
        var cur = this
        a match {
          case x: Identifier => cur = cur.add(x, left.onlyIf(right))
          case _ => ()
        }
        b match {
          case x: Identifier => cur = cur.add(x, right.onlyIf(left))
          case _ => ()
        }
        cur

      case BinaryArithmeticExpression(a, b, ArithmeticOperator.!=, _) =>

        val left = eval(a)
        val right = eval(b)
        if (left.mustBeInvalid && right.mustBeInvalid)
          return bottom()
        var cur = this
        if (right.mustBeInvalid && left.canBeInvalid) {
          a match {
            case x: Identifier => cur = cur.add(x, left.onlyIf(Valid))
            case _ => ()
          }
        }
        if (left.mustBeInvalid && right.canBeInvalid) {
          b match {
            case x: Identifier => cur = cur.add(x, right.onlyIf(Valid))
            case _ => ()
          }
        }
        cur

      case _ => this
    }

    res
  }

  override def toString: String = {
    if (isBottom) return "⊥"
    var result: String = ""
    map.foreach { case (k, v) =>
      if (v.canBeInvalid)
        if (v.canBeValid)
          result += k.toString + " may be invalid\n"
        else
          result += k.toString + " is invalid\n"
      else if (v.canBeValid)
        result += k.toString + " is valid\n"
      else
        result += k.toString + " is ⊥\n"
    }
    result
  }

  override def glb(other: BooleanInvalidDomainWithSource): BooleanInvalidDomainWithSource = {
    val r = super.glb(other)
    if (r.isTop || r.isBottom) return r

    /*
      To find false alarms, we have to make the state bottom for glb(a,b)
      if there is an id s.t.  a(id) = Invalid, b(id) = Valid, or vice versa. The functional domain by default only
      sets invidivdual entry glb(a,b)(id) = Bottom, not the whole functional domain.
     */
    val funcMap = r.map
    // safer check but should not be necessary anymore (and less general):
    //  funcMap.exists({case (id, validity) => validity.isBottom && !this.get(id).isBottom && !other.get(id).isBottom})
    if (funcMap.values.exists(_.isBottom)) {
      r.bottom()
    } else r
  }

  /**
    * May try to explain an error
    *
    * @param expr An error-expression that should be infeasible but exposes an error
    * @return If a cause of the error is found, it returns an explanation and the program point of the cause
    */
  override def explainError(expr: Expression): Set[(String, ProgramPoint)] = {
    val res: Set[(String, ProgramPoint)] = expr match {
      case BinaryArithmeticExpression(a: Identifier, InvalidExpression(_, _, _), ArithmeticOperator.==, _) =>

        val left = eval(a)
        left match {
          case ValiditySet.Inner(set) =>
            set flatMap {
              case InvalidDomainValue(explanation, pp) => Some(explanation, pp)
              case _ => None
            }
          case _ => Set.empty
        }

      case _ => Set.empty
    }

    res
  }

  override def getPossibleConstants(id: Identifier) = SetDomain.Default.Top()
}


/**
  * Attaches a position to every invalid value
  *
  * @author Lucas Brutschy
  */
trait ValiditySet extends SetDomain[ValiditySetValue, ValiditySet] {

  def bottom() = ValiditySet.Bottom

  def top() = ValiditySet.Top

  def factory(value: Set[ValiditySetValue]) =
    if (value.isEmpty) ValiditySet.Bottom else ValiditySet.Inner(value)

  def canBeInvalid: Boolean

  def canBeValid: Boolean

  def mustBeInvalid: Boolean

  def mustBeValid: Boolean

  def onlyIf(x: ValiditySet): ValiditySet

}

object ValiditySet {

  def Invalid(explanation: String, pp: ProgramPoint) = Inner(Set(InvalidDomainValue(explanation, pp)))

  lazy val Valid = Inner(Set(ValidDomainValue()))

  object Bottom extends ValiditySet
    with SetDomain.Bottom[ValiditySetValue, ValiditySet] {

    def canBeInvalid = false

    def canBeValid = false

    def mustBeInvalid = false

    def mustBeValid = false

    def onlyIf(x: ValiditySet) = this

    override def toString = "⊥"

  }

  object Top extends ValiditySet
    with SetDomain.Top[ValiditySetValue, ValiditySet] {

    def canBeInvalid = true

    def canBeValid = true

    def mustBeInvalid = false

    def mustBeValid = false

    def onlyIf(x: ValiditySet) = this

    override def toString = "Valid or Invalid with unknown cause"
  }

  case class Inner(value: Set[ValiditySetValue] = Set.empty[ValiditySetValue])
    extends ValiditySet
      with SetDomain.Inner[ValiditySetValue, ValiditySet, Inner] {

    def canBeInvalid = value.exists { case InvalidDomainValue(_, _) => true; case _ => false }

    def canBeValid = value.exists { case ValidDomainValue() => true; case _ => false }

    def mustBeInvalid = canBeInvalid && !canBeValid

    def mustBeValid = canBeValid && !canBeInvalid

    def onlyIf(x: ValiditySet) = {
      var res: ValiditySet = ValiditySet.Bottom
      if (x.canBeValid) res = res lub factory(value.collect { case x@ValidDomainValue() => x })
      if (x.canBeInvalid) res = res lub factory(value.collect { case x@InvalidDomainValue(_, _) => x })
      res
    }

    override def toString: String = {
      if (mustBeValid) return "Valid"
      value.map({
        case ValidDomainValue() => "Valid"
        case InvalidDomainValue(cause, ss) => "Invalid, since " + cause + " at " + ss
      }).mkString(" or ")
    }

  }

  trait ValiditySetValue

  case class ValidDomainValue() extends ValiditySetValue

  case class InvalidDomainValue(explanation: String, source: ProgramPoint) extends ValiditySetValue

}