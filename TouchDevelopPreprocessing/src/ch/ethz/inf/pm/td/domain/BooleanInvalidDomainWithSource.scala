package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.{AbstractOperator, BinaryBooleanExpression, BinaryNondeterministicExpression, Constant, NegatedBooleanExpression, _}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.td.domain.PositionedInvalidValueDomain._
import ch.ethz.inf.pm.td.semantics.TBoolean


/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:50 AM
 * 
 */
class BooleanInvalidDomainWithSource (val map:Map[Identifier, PositionedInvalidValueDomain] = Map.empty[Identifier, PositionedInvalidValueDomain],
                                      override val isBottom:Boolean = false,
                                      val isTop:Boolean = false)
  extends BoxedDomain[PositionedInvalidValueDomain,BooleanInvalidDomainWithSource]
  with InvalidDomain[BooleanInvalidDomainWithSource] {

  def functionalFactory(_value:Map[Identifier, PositionedInvalidValueDomain] = Map.empty[Identifier, PositionedInvalidValueDomain],
                        _isBottom:Boolean = false,
                        _isTop:Boolean = false) : BooleanInvalidDomainWithSource =
    new BooleanInvalidDomainWithSource(_value,_isBottom,_isTop)

  def get(key : Identifier) : PositionedInvalidValueDomain = map.get(key) match {
    case None => domBottom
    case Some(x) => x
  }

  override def createVariable(variable: Identifier, typ: Type): BooleanInvalidDomainWithSource = {
    this
  }

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    var result = Map.empty[Identifier, List[String]]
    result = result + ((variable, path ::: variable.toString :: Nil))
    (this.add(variable, domTop), result)
  }

  override def setToTop(variable: Identifier): BooleanInvalidDomainWithSource = {
    // Check necessary, otherwise bottomness of state is lost
    if (this.isBottom) return this

    this.add(variable, domTop)
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
    val res = eval(expr)
    if (!res.isTop && (res.isBottom || res.value.isEmpty))
      bottom()
    else if (variable.representsSingleVariable) this.add(variable, res)
    else this.add(variable, get(variable).lub(res))
  }

  override def backwardAssign(oldPreState: BooleanInvalidDomainWithSource, variable: Identifier, expr: Expression): BooleanInvalidDomainWithSource = {
    val s = assume(BinaryArithmeticExpression(variable, expr, ArithmeticOperator.== , TBoolean))
    s.setToTop(variable)
  }

  override def access(field: Identifier) = this

  override def backwardAccess(field: Identifier) = this

  /**
   * Evaluation for invalid values. The following rules apply:
   *
   * 1) For every arithmetic operation, the expression may be invalid if one of the two operands may be invalid.
   *    The expression must be invalid, if one of the two operands must be invalid.
   * 2) Invalid constant = invalid, all other constants = valid
   *
   */
  private def eval(expr: Expression): PositionedInvalidValueDomain = expr match {
    case BinaryArithmeticExpression(left, right, _, typ) => eval(left).lub(eval(right))
    case BinaryNondeterministicExpression(left, right, _, typ) => eval(left).lub(eval(right))
    case AbstractOperator(left,List(right),Nil,AbstractOperatorIdentifiers.stringConcatenation,_) => eval(left).lub(eval(right))
    case InvalidExpression(typ, str, pp) => domInvalid(str, pp)
    case ValidExpression(typ,pp) => domValid
    case Constant(_, _, _) => domValid
    case h: HeapIdentifier => domValid
    case x: Identifier => this.get(x)
    case xs: HeapIdSetDomain[_] =>
      var result = domBottom
      for (x <- xs.value) result = result.lub(get(x))
      result
    case x: Expression => domTop
  }

  /**
   * We only implement the most necessary part of this (completing this should be quite hard)
   * E ::= x == invalid | x != invalid | E and E | E or E
   *
   * TODO: We would have to convert this to some normal Form
   * TODO: Case x == y, x != y
   */
  override def assume(expr: Expression): BooleanInvalidDomainWithSource = {
    val res = expr match {
      // double negation (happens e.g. when using  "not foo->is_invalid")
      case NegatedBooleanExpression(NegatedBooleanExpression(x)) => {
        assume(x)
      }
      case BinaryArithmeticExpression(a:Expression, b:Expression, ArithmeticOperator.==, _) =>

        val left = eval(a)
        val right = eval(b)
        if ((left.mustBeValid && right.mustBeInvalid) || (left.mustBeInvalid && right.mustBeValid))
          return bottom()
        var cur = this
        a match {
          case x:Identifier => cur = cur.add(x,left.onlyIf(right))
          case xs: HeapIdSetDomain[_] => for (x <- xs.value) cur = cur.add(x, left.onlyIf(right))
          case _ => ()
        }
        b match {
          case x:Identifier => cur = cur.add(x,right.onlyIf(left))
          case xs: HeapIdSetDomain[_] => for (x <- xs.value) cur = cur.add(x, right.onlyIf(left))
          case _ => ()
        }
        cur

      case NegatedBooleanExpression(BinaryArithmeticExpression(a,b, ArithmeticOperator.==, _)) =>

        val left = eval(a)
        val right = eval(b)
        if (left.mustBeInvalid && right.mustBeInvalid)
          return bottom()
        var cur = this
        if (right.mustBeInvalid && left.canBeInvalid) {
          a match {
            case x:Identifier => cur = cur.add(x,left.onlyIf(domValid))
            case xs: HeapIdSetDomain[_] => for (x <- xs.value) cur = cur.add(x, left.onlyIf(domValid))
            case _ => ()
          }
        }
        if (left.mustBeInvalid && right.canBeInvalid) {
          b match {
            case x:Identifier => cur = cur.add(x,right.onlyIf(domValid))
            case xs: HeapIdSetDomain[_] => for (x <- xs.value) cur = cur.add(x, right.onlyIf(domValid))
            case _ => ()
          }
        }
        cur

      case BinaryBooleanExpression(left,right,op,typ) => op match {
        case BooleanOperator.&& => assume(left).assume(right)
        case BooleanOperator.|| => assume(left).lub(assume(right))
      }
      case _ => this
    }

    res
  }

  override def toString:String = {
    if (isBottom) return "⊥"
    var result : String = ""
    map.foreach { case (k,v) =>
      if(v.canBeInvalid)
        if(v.canBeValid)
          result += k.toString+" may be invalid\n"
        else
          result += k.toString+" is invalid\n"
      else
        if(v.canBeValid)
          result += k.toString+" is valid\n"
        else
          result += k.toString+" is ⊥\n"
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
    val res : Set[(String, ProgramPoint)] = expr match {
      case BinaryArithmeticExpression(a: Identifier, InvalidExpression(_, _, _), ArithmeticOperator.==, _) =>

        val left = eval(a)
        left.value flatMap {
          case InvalidDomainValue(explanation, pp) => Some(explanation, pp)
          case _ => None
        }

      case _ => Set.empty
    }

    res
  }

}


object PositionedInvalidValueDomain {

  // Helper values
  lazy val domBottom = new PositionedInvalidValueDomain().bottom()

  def domInvalid(explanation: String, pp: ProgramPoint) = new PositionedInvalidValueDomain().add(InvalidDomainValue(explanation, pp))

  lazy val domValid = new PositionedInvalidValueDomain().add(ValidDomainValue())
  lazy val domTop = new PositionedInvalidValueDomain().top()

}

/**
 * Attaches a position to every invalid value
 *
 * Lucas Brutschy
 * Date: 06/09/13
 * Time: 10:49 AM
 */
case class PositionedInvalidValueDomain(
                                         value: Set[ValidnessDomainValue] = Set.empty[ValidnessDomainValue],
    isTop: Boolean = false,
    isBottom: Boolean = false)
  extends SetDomain[ValidnessDomainValue, PositionedInvalidValueDomain] {

  def setFactory(
                  value: Set[ValidnessDomainValue] = Set.empty[ValidnessDomainValue],
      isTop: Boolean = false,
      isBottom: Boolean = false) =
    PositionedInvalidValueDomain(value, isTop, isBottom)

  def canBeInvalid = isTop || value.exists { case InvalidDomainValue(_, _) => true; case _ => false}

  def canBeValid = isTop || value.exists { case ValidDomainValue() => true; case _ => false}
  def mustBeInvalid = canBeInvalid && !canBeValid
  def mustBeValid = canBeValid && !canBeInvalid

  // intersection corresponds to glb
  def onlyIf(x : PositionedInvalidValueDomain):PositionedInvalidValueDomain = {
    if (x.isTop) return this
    if (this.isTop) return this
    var res = new PositionedInvalidValueDomain()
    if (x.canBeValid) res = res.setFactory(res.value ++ this.value.collect { case x@ValidDomainValue() => x})
    if (x.canBeInvalid) res = res.setFactory(res.value ++ this.value.collect { case x@InvalidDomainValue(_, _) => x})
    if (res.value.isEmpty) return bottom()
    res
  }

  override def toString:String = {
    if (isTop) return "Valid or Invalid with unknown cause"
    if (isBottom || value.isEmpty) return "Bottom"
    if (mustBeValid) return "Valid"
    value.map({
      case ValidDomainValue() => "Valid"
      case InvalidDomainValue(cause, ss) => "Invalid, since " + cause + " at " + ss
    }).mkString(" or ")
  }

}

trait ValidnessDomainValue

case class ValidDomainValue() extends ValidnessDomainValue

case class InvalidDomainValue(explanation: String, source: ProgramPoint) extends ValidnessDomainValue