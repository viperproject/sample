/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import javax.naming.OperationNotSupportedException

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._

/** Arithmetic operators. */
object ArithmeticOperator extends Enumeration {
  val + = Value("+")
  val - = Value("-")
  val * = Value("*")
  val / = Value("/")
  val % = Value("%")
  val >= = Value(">=")
  val <= = Value("<=")
  val == = Value("==")
  val != = Value("!=")
  val > = Value(">")
  val < = Value("<")

  def isArithmetic(op: Value): Boolean = !isComparison(op)

  def isComparison(op: Value): Boolean = Set(>=, <=, ==, !=, >, <) contains op

  def returnTyp(op: Value, left: Type, right: Type): Type = {
    if (isComparison(op)) SystemParameters.tm.Boolean
    else left.lub(right)
  }

  /**
   * Negates the given given arithmetic operator if possible.
   * @param op the operator to negate
   * @throws MatchError if the operator cannot be negated
   */
  def negate(op: Value): Value = op match {
    case `<=` => `>`
    case `<` => `>=`
    case `>=` => `<`
    case `==` => `!=`
    case `!=` => `==`
    case `>` => `<=`
  }

  /**
   * Flips the given arithmetic operator if possible.
   * @param op the operator to flip
   * @throws MatchError if the operator cannot be flipped
   */
  def flip(op: Value): Value = op match {
    case `+` => ArithmeticOperator.`+` // Make the compiler happy
    case `*` => `*`
    case `>=` => `<=`
    case `<=` => `>=`
    case `==` => `==`
    case `!=` => `!=`
    case `>` => `<`
    case `<` => `>`
  }
}

/** Boolean operators. */
object BooleanOperator extends Enumeration {
  val && = Value("&&")
  val || = Value("||")

  def negate(op: Value): Value = op match {
    case `&&` => `||`
    case `||` => `&&`
  }
}

/**
  * Binary operators for comparing references
  */
object ReferenceOperator extends Enumeration {

  val == = Value("==")
  val != = Value("!=")

  /**
    * Negates the given given reference operator if possible.
    *
    * @param op the operator to negate
    * @throws MatchError if the operator cannot be negated
    */
  def negate(op: Value): Value = op match {
    case `==` => `!=`
    case `!=` => `==`
  }

  def returnTyp(op: Value, left: Type, right: Type): Type = {
    SystemParameters.tm.Boolean
  }

}


/**
 * Abstract operators that can be used to represent any operations on different types of objects, like string concatenation and type casts
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
object AbstractOperatorIdentifiers extends Enumeration {
  val isInstanceOf = Value("isInstanceOf")
  val asInstanceOf = Value("asInstanceOf")
  val == = Value("==")
  val stringConcatenation = Value("StringConcat")
  val stringIndexof = Value("StringIndexof")
  val stringLastindexof = Value("StringLastindexof")
  val stringSubstring = Value("StringSubstring")
  val stringContains = Value("StringContains")
  val arrayApply = Value("arrayApply")
}


/** Expressions are used to represents the values returned by statements.
  *
  * For instance, an assignment returns a UnitExpression, while a variable access returns a VariableIdentifier
  * and a field access returns a heap identifier.
  * Expressions represent the results of method calls and arithmetic and boolean operations as well.
  *
  * @author Pietro Ferrara, Lucas Brutschy
  * @since 0.1
  */
trait Expression {

  /** Whether the expression does not contain conjunctions, disjunctions or negations. */
  var canonical: Boolean = false

  /** The type of this expression. */
  def typ: Type

  /** Point in the program where this expression is located. */
  def pp: ProgramPoint

  /** All identifiers that are part of this expression. */
  def ids: IdentifierSet

  /** Replace one identifier by another in this expression (and all sub-expressions)
    *
    * @param a The identifier to be replaced
    * @param b The replacement identifier
    * @return An replaced version of this expression
    */
  def replace(a: Identifier, b: Identifier): Expression = {
    transform({
      case x: Identifier => if (x.equals(a)) b else x;
      case x: Expression => x
    })
  }

  /** Runs f on the expression and all sub-expressions
    *
    * This also replaces identifiers inside heap ID sets.
    *
    * @param f the transformer
    * @return the transformed expression
    */
  def transform(f: (Expression => Expression)): Expression

  /** Checks if function f evaluates to true for any sub-expression. */
  def contains(f: (Expression => Boolean)): Boolean

}


/** The negation of an expression. */
case class NegatedBooleanExpression(exp: Expression) extends Expression {
  def typ: Type = exp.typ

  def pp: ProgramPoint = exp.pp

  def ids: IdentifierSet = exp.ids

  override def toString = s"! $exp"

  def transform(f: (Expression => Expression)): Expression =
    f(NegatedBooleanExpression(exp.transform(f)))

  def contains(f: (Expression => Boolean)): Boolean = f(this) || exp.contains(f)
}

/**
 * Represents a generic operation, e.g., concatenation of strings. Usually, at source code level these
 * operations are encoded as method calls. 
 *
 * @param thisExpr The object on which the expression is performed
 * @param parameters A (possibly empty) list of arguments
 * @param typeparameters A (possibly empty) list of generic types
 * @param op The identifier of the operation
 * @param returntyp The type of the value returned by the operation
 * @author Pietro Ferrara
 * @since 0.1
 */
case class AbstractOperator(
                             thisExpr: Expression,
                             parameters: List[Expression],
                             typeparameters: List[Type],
                             op: AbstractOperatorIdentifiers.Value,
                             returntyp: Type) extends Expression {

  def pp: ProgramPoint = thisExpr.pp

  def typ: Type = returntyp

  def ids: IdentifierSet = thisExpr.ids ++ {
    var result: IdentifierSet = IdentifierSet.Bottom
    for (p <- parameters) {
      result ++= p.ids
    }
    result
  }

  override def hashCode(): Int = thisExpr.hashCode()

  override def equals(o: Any): Boolean = o match {
    case AbstractOperator(l, p, t, opx, _) => thisExpr.equals(l) && parameters.equals(p) && typeparameters.equals(t) & op.equals(opx)
    case _ => false
  }

  override def toString: String = thisExpr.toString + "." + op.toString + ToStringUtilities.parametricTypesToString(typeparameters) + "(" + ToStringUtilities.listToString(parameters) + ")"

  override def transform(f: (Expression => Expression)): Expression =
    f(AbstractOperator(thisExpr.transform(f), parameters.map(_.transform(f)), typeparameters, op, returntyp))

  def contains(f: (Expression => Boolean)): Boolean = f(this) || thisExpr.contains(f) || parameters.map(_.contains(f)).foldLeft(false)(_ || _)
}

/**
 * A binary boolean expression, e.g. B1 && B2
 *
 * @param left One of the operands
 * @param right The other operand
 * @param op The identifier of the operation
 * @author Pietro Ferrara
 * @since 0.1
 */
case class BinaryBooleanExpression(
                                    left: Expression,
                                    right: Expression,
    op: BooleanOperator.Value) extends Expression {

  def pp: ProgramPoint = left.pp

  def typ: Type = SystemParameters.tm.Boolean

  def ids: IdentifierSet = left.ids ++ right.ids

  override def hashCode(): Int = left.hashCode()

  override def equals(o: Any): Boolean = o match {
    case BinaryBooleanExpression(l, r, opx) => left.equals(l) && right.equals(r) && op.equals(opx)
    case _ => false
  }

  override def toString: String = "(" + left.toString + op.toString + right.toString + ")"

  override def transform(f: (Expression => Expression)): Expression =
    f(BinaryBooleanExpression(left.transform(f), right.transform(f), op))

  def contains(f: (Expression => Boolean)): Boolean = f(this) || left.contains(f) || right.contains(f)

}


trait BinaryExpression extends Expression {

  def left: Expression

  def right: Expression

  def ids: IdentifierSet = left.ids ++ right.ids

  def pp: ProgramPoint = left.pp

  def contains(f: (Expression => Boolean)): Boolean = f(this) || left.contains(f) || right.contains(f)

}


/**
 * A comparison between reference, that is, left == right or left != right
 *
 * @param left One of the operands
 * @param right The other operand
 * @param op The identifier of the operation
 * @author Pietro Ferrara
 * @since 0.1
 */
case class ReferenceComparisonExpression(
                                          left: Expression,
                                          right: Expression,
    op: ReferenceOperator.Value) extends BinaryExpression {

  require(left.typ.isObject,
    "cannot perform reference comparisons on primitive values")
  require(right.typ.isObject,
    "cannot perform reference comparisons on primitive values")

  def typ: Type = ReferenceOperator.returnTyp(op, left.typ, right.typ)

  override def toString = s"$left$op$right"

  override def transform(f: (Expression => Expression)): Expression =
    f(copy(left = left.transform(f), right = right.transform(f)))


}

/**
 * A binary arithmetic expression, e.g. A1+A2 or A1>=A2
 *
 * @param left One of the operands
 * @param right The other operand
 * @param op The identifier of the operation
 * @author Pietro Ferrara
 * @since 0.1
 */
case class BinaryArithmeticExpression(
                                       left: Expression,
                                       right: Expression,
    op: ArithmeticOperator.Value) extends BinaryExpression {

  def typ: Type = ArithmeticOperator.returnTyp(op, left.typ, right.typ)

  override def toString: String = left.toString + op.toString + right.toString

  override def transform(f: (Expression => Expression)): Expression =
    f(BinaryArithmeticExpression(left.transform(f), right.transform(f), op))

}

object BinaryArithmeticExpression {

  /**
   * Creates an expression that represents the concatenation
   * of a sequence of expressions with a certain arithmetic operator.
   *
   * @param exps the sequence of expressions to concatenate
   * @param op the arithmetic operator to concatenate the expressions with
   * @param emptyExp the expression to return if `exps` is empty
   */
  def apply(exps: Iterable[Expression],
            op: ArithmeticOperator.Value,
            emptyExp: Expression): Expression =
    if (exps.isEmpty) emptyExp
    else exps.reduceLeft(BinaryArithmeticExpression(_, _, op))

}

/**
 * A unary arithmetic expression, e.g. -A1
 *
 * @param left The operand
 * @param op The identifier of the operation
 * @param returntyp The type of the returned value
 * @author Pietro Ferrara
 * @since 0.1
 */
case class UnaryArithmeticExpression(left: Expression, op: ArithmeticOperator.Value, returntyp: Type) extends Expression {

  def pp: ProgramPoint = left.pp

  def typ: Type = returntyp

  def ids: IdentifierSet = left.ids

  override def toString: String = op.toString + left.toString

  override def transform(f: (Expression => Expression)): Expression =
    f(UnaryArithmeticExpression(left.transform(f), op, returntyp))

  def contains(f: (Expression => Boolean)): Boolean = f(this) || left.contains(f)

}

/**
 * A (usually numeric) constant represented by a string 
 *
 * @param constant The constant
 * @param typ The type of the constant
 * @author Pietro Ferrara
 * @since 0.1
 */
case class Constant(
                     constant: String,
    typ: Type,
                     pp: ProgramPoint = DummyProgramPoint)
  extends Expression {

  def ids = IdentifierSet.Bottom

  override def hashCode(): Int = constant.hashCode()

  override def equals(o: Any): Boolean = o match {
    case Constant(c, t, _) => constant.equals(c) && typ.equals(t)
    case _ => false
  }

  override def toString: String = constant

  override def transform(f: (Expression => Expression)): Expression = f(this)

  def contains(f: (Expression => Boolean)): Boolean = f(this)

}

/**
 * An identifier, that could be a variable or a node of the abstract heap.
 */
trait Identifier extends Expression with Assignable {

  def ids = IdentifierSet.Inner(Set(this))

  def transform(f: (Expression => Expression)): Expression = f(this)

  /**
   * Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
   * if they are the same identifier
   * @return The name of the identifier
   */
  def getName: String

  /**
   * Returns the name of the field that is represented by this identifier if it is a heap identifier.
   *
   * @return The name of the field pointed by this identifier
   */
  def getField: Option[String]

  /**
   * Since an abstract identifier can be an abstract node of the heap, it can represent more than one concrete
   * identifier. This function tells if a node is a summary node.
   *
   * @return true iff this identifier represents exactly one variable
   */
  def representsSingleVariable: Boolean

  override def toString: String = getName

  def sanitizedName: String = getName.replaceAll("[^a-z0-9A-Z]*","")

  def contains(f: (Expression => Boolean)): Boolean = f(this)
}

object Identifier {

  trait HeapIdentifier extends Identifier

  trait FieldIdentifier extends Identifier {

    def obj:Identifier.HeapIdentifier
    def field:String

  }

}

/**
 * An identifier for identifying a scope
 */
trait ScopeIdentifier

/**
 * If you do not care about scopes.
 */
object EmptyScopeIdentifier extends ScopeIdentifier {
  override def toString = ""
}

/**
 * Scopes identified by a program point, e.g. method-scope identified by program point of method declaration
 *
 * @param pp the program point of the beginning of the scope
 */
case class ProgramPointScopeIdentifier(pp: ProgramPoint) extends ScopeIdentifier {

  override def hashCode(): Int = pp.hashCode()

  override def equals(o: Any): Boolean = o match {
    case ProgramPointScopeIdentifier(oPP) => pp.equals(oPP)
    case _ => false
  }

  override def toString: String = "@" + pp.toString

}

/**
 * The identifier of a variable.
 *
 * @param name The name of the variable
 * @param typ The type of the variable
 */
case class VariableIdentifier
(name: String, scope: ScopeIdentifier = EmptyScopeIdentifier)
(val typ: Type, val pp: ProgramPoint = DummyProgramPoint)
  extends Identifier {

  require(typ != null)

  override def toString: String = getName

  override def getName: String = name.toString + scope.toString

  override def getField = None

  // Variables always represent exactly one concrete identifier
  override def representsSingleVariable = true
}

/**
 * The heap identifier that has to be implemented by particular heap analyses.
 */
trait HeapIdentifier[I <: HeapIdentifier[I]] extends Identifier.HeapIdentifier {}

/**
 * The unit expression, that represents the absence of a concrete expression.
 *
 * @param typ The unit type
 * @author Pietro Ferrara
 * @since 0.1
 */
case class UnitExpression(typ: Type, pp: ProgramPoint) extends Expression {
  def ids = IdentifierSet.Bottom

  override def hashCode(): Int = 0

  override def equals(o: Any): Boolean = o match {
    case UnitExpression(_, _) => true
    case _ => false
  }

  override def toString = "Unit"

  override def transform(f: (Expression => Expression)): Expression = f(this)

  def contains(f: (Expression => Boolean)): Boolean = f(this)
}

case class AccessPathIdentifier(path: List[Identifier])
  extends Identifier {

  require(path.nonEmpty, "the access path must not be empty")

  def getField = throw new OperationNotSupportedException()

  def pp: ProgramPoint = path.last.pp

  def representsSingleVariable: Boolean = true

  override def toString: String = getName

  def getName: String = stringPath.mkString(".")

  def stringPath: List[String] =
    path.map(_.getName)

  def objPath: List[String] =
    if (typ.isObject) stringPath else stringPath.dropRight(1)

  def typ: Type = path.last.typ
}

object AccessPathIdentifier {
  /** Constructs an access path identifier from a given variable identifier. */
  def apply(id: VariableIdentifier): AccessPathIdentifier =
    AccessPathIdentifier(List[Identifier](id))

  /** Constructs an access path identifier from a receiver object access path
    * and a field identifier.
    */
  def apply(objPath: List[Identifier], field: Identifier): AccessPathIdentifier =
    AccessPathIdentifier(objPath ++ List(field))
}

/**
 * Implements non-deterministic operations. These can be used to represent non-deterministic expressions on the
 * stack of the state, e.g. the result of a math.random call.
 *
 * Note that not every numerical domain has direct support for this.
 *
 * @author Lucas Brutschy
 */
object NondeterministicOperator extends Enumeration {

  /**
   * Represents a nondeterministic choice between two expressions, e.g (a+3) or (b+3) or (invalid)
   */
  val or = Value("or")

  /**
   * Represents a nondeterministic choice in a given range (interval), e.g. 1 to (x+y). Includes upper bound
   */
  val toIncl = Value("toIncl")

  /**
   * Represents a nondeterministic choice in a given range (interval), e.g. 1 to (x+y). Excludes upper bound
   */
  val toExcl = Value("toExcl")

  def returnTyp(op: Value, left: Type, right: Type): Type = {
    left.lub(right)
  }
}

/**
 * Represents an expression with a nondeterministic operator.
 *
 * @author Lucas Brutschy
  */
case class BinaryNondeterministicExpression(left: Expression, right: Expression, op: NondeterministicOperator.Value) extends BinaryExpression {

  def typ: Type = NondeterministicOperator.returnTyp(op, left.typ, right.typ)

  override def toString: String = left.toString + " " + op.toString + " " + right.toString

  override def transform(f: (Expression => Expression)): Expression =
    f(BinaryNondeterministicExpression(left.transform(f), right.transform(f), op))
}

// CUSTOM EXPRESSIONS FOR QUANTIFIED PERMISSION ANALYSIS

case class ForallExpression(leftCond: Expression, right: Expression, quantifiedVariable: VariableIdentifier) extends
  Expression {
  /** The type of this expression. */
  override def typ: Type = leftCond.typ

  /** Point in the program where this expression is located. */
  override def pp: ProgramPoint = leftCond.pp

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = leftCond.ids ++ right.ids

  /** Runs f on the expression and all sub-expressions
    *
    * This also replaces identifiers inside heap ID sets.
    *
    * @param f the transformer
    * @return the transformed expression
    */
  override def transform(f: (Expression) => Expression): Expression = f(ForallExpression(leftCond.transform(f), right.transform(f), quantifiedVariable))

  /** Checks if function f evaluates to true for any sub-expression. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this) || leftCond.contains(f) || right.contains(f)
}

case class FieldExpression(typ: Type, field: String, receiver: Expression) extends Expression {

  /** Point in the program where this expression is located. */
  override def pp: ProgramPoint = receiver.pp

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = receiver.ids

  /** Runs f on the expression and all sub-expressions
    *
    * This also replaces identifiers inside heap ID sets.
    *
    * @param f the transformer
    * @return the transformed expression
    */
  override def transform(f: (Expression) => Expression): Expression = f(FieldExpression(typ, field, receiver.transform(f)))

  /** Checks if function f evaluates to true for any sub-expression. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this) || receiver.contains(f)
}

  /**
    * A field access predicate used in inhale and exhale expressions.
    *
    * @param location    The location for which we inhale or exhale the permission.
    * @param numerator   The numerator of the inhaled or exhaled permission.
    * @param denominator The denominator of the inhaled or exhaled permission.
    * @param typ         The type of the field access predicate.
    * @author Caterina Urban
    */
case class FieldAccessPredicate(location: Expression, numerator: Expression, denominator: Expression, typ: Type)
  extends Expression
{
  override def transform(f: (Expression) => Expression): Expression = f(FieldAccessPredicate(location.transform(f), numerator.transform(f), denominator.transform(f), typ))

  override def ids: IdentifierSet = location.ids ++ numerator.ids ++ denominator.ids

  override def pp: ProgramPoint = location.pp

  override def contains(f: (Expression) => Boolean): Boolean = f(this) || location.contains(f) || numerator.contains(f) || denominator.contains(f)
}

/**
  * A permission expression returning the current amount of permission for
  * the location specified in the argument.
  *
  * @param location The location of the permission expression.
  * @param typ      The type of the permission expression.
  * @author Jerome Dohrau
  */
case class CurrentPermission(location: Expression, typ: Type)
  extends Expression
{
  override def ids: IdentifierSet = location.ids

  override def pp: ProgramPoint = location.pp

  override def transform(f: (Expression) => Expression): Expression = f(CurrentPermission(location.transform(f), typ))

  override def contains(f: (Expression) => Boolean): Boolean = f(this) || location.contains(f)
}

case class ConditionalExpression(cond: Expression, left: Expression, right: Expression, typ: Type) extends Expression {
  /** Point in the program where this expression is located. */
  override def pp: ProgramPoint = cond.pp

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = cond.ids ++ left.ids ++ right.ids

  /** Runs f on the expression and all sub-expressions
    *
    * This also replaces identifiers inside heap ID sets.
    *
    * @param f the transformer
    * @return the transformed expression
    */
  override def transform(f: (Expression) => Expression): Expression = f(ConditionalExpression(cond.transform(f), left.transform(f), right.transform(f), typ))

  /** Checks if function f evaluates to true for any sub-expression. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this) || cond.contains(f) || left.contains(f) || right.contains(f)
}


/**
  *
  * Represents equalities/inequalities between strings
  *
  * @author Lucas Brutschy
  *
  */
case class BinaryStringExpression(left: Expression, right: Expression, op: StringOperator.Value) extends BinaryExpression {

  assert(left.typ.isStringType)
  assert(right.typ.isStringType)

  override def typ: Type = SystemParameters.tm.Boolean

  override def transform(f: (Expression) => Expression): Expression = {
    f(this.copy(left.transform(f), right.transform(f)))
  }

}

/**
  * An expression that represents a function call.
  *
  * @param functionName The name of the called function.
  * @param parameters   A (possibly empty) sequence of expressions corresponding to the passed parameters in the function call.
  * @param typ          The return type of the function.
  * @param pp           The program point identifying the location of the function call.
  * @author Severin Münger
  */
case class FunctionCallExpression(functionName: String, parameters: Seq[Expression] = Seq(), typ: Type, pp: ProgramPoint = DummyProgramPoint)
  extends Expression {

  override def ids: IdentifierSet = parameters.foldLeft[IdentifierSet](IdentifierSet.Bottom)((ids, param) => ids ++ param.ids)

  override def transform(f: (Expression) => Expression): Expression = f(FunctionCallExpression(functionName, parameters.map(param => param.transform(f)), typ, pp))

  override def contains(f: (Expression) => Boolean): Boolean = f(this) || parameters.exists(param => param.contains(f))
}

object StringOperator extends Enumeration {

  val == = Value("==")
  val != = Value("!=")

  def negate(op: Value): Value = op match {
    case `==` => `!=`
    case `!=` => `==`
  }

}

trait TypeMap {

  val Int: Type

  val Float: Type

  val String: Type

  val Boolean: Type

  val Bottom: Type

  val Top: Type

}

object ExpressionFactory {

  def BigOr(expressions: List[Expression])(implicit pp: ProgramPoint): Expression = {
    expressions match {
      case Nil => True
      case List(head) => head
      case he :: tail => he || BigOr(tail)
    }
  }

  def BigAnd(expressions: List[Expression])(implicit pp: ProgramPoint): Expression = {
    expressions match {
      case Nil => True
      case List(head) => head
      case he :: tail => he && BigAnd(tail)
    }
  }

  @inline def True(implicit pp: ProgramPoint): Expression =
    Constant("true", SystemParameters.tm.Boolean, pp)

  @inline def Var(name: String, typ: Type)(implicit pp: ProgramPoint): Expression =
    VariableIdentifier(name)(typ, pp)

  @inline def IntVar(name: String)(implicit pp: ProgramPoint): Expression =
    VariableIdentifier(name)(SystemParameters.tm.Int, pp)

  @inline def BoolVar(name: String)(implicit pp: ProgramPoint): Expression =
    VariableIdentifier(name)(SystemParameters.tm.Boolean, pp)

  @inline def StringVar(name: String)(implicit pp: ProgramPoint): Expression =
    VariableIdentifier(name)(SystemParameters.tm.String, pp)

  @inline def BinaryNumNum(a: Expression, b: Expression, op: ArithmeticOperator.Value): Expression = {
    assert(a.typ.isNumericalType && b.typ.isNumericalType)
    BinaryArithmeticExpression(a, b, op)
  }

  @inline def BinaryStrBool(a: Expression, b: Expression, op: ArithmeticOperator.Value): Expression = {
    assert(a.typ.isStringType && b.typ.isStringType)
    BinaryArithmeticExpression(a, b, op)
  }

  @inline def BinaryNumBool(a: Expression, b: Expression, op: ArithmeticOperator.Value): Expression = {
    assert(a.typ.isNumericalType && b.typ.isNumericalType)
    BinaryArithmeticExpression(a, b, op)
  }

  @inline def BinaryBoolBool(a: Expression, b: Expression, op: BooleanOperator.Value): Expression = {
    assert(a.typ.isBooleanType && b.typ.isBooleanType && a.typ == b.typ)
    BinaryBooleanExpression(a, b, op)
  }

  @inline def -(expr: Expression): Expression = {
    UnaryArithmeticExpression(expr, ArithmeticOperator.-, expr.typ)
  }

  @inline implicit def toRichExpression(e: Expression): RichExpression = RichExpression(e)

  @inline implicit def toExpression(e: RichExpression): Expression = e.expr

  @inline implicit def toRichExpression(e: Int)(implicit pp: ProgramPoint): RichExpression =
    RichExpression(Constant(e.toString, SystemParameters.tm.Int, pp))

  @inline implicit def toRichExpression(e: String)(implicit pp: ProgramPoint): RichExpression =
    RichExpression(Constant(e.toString, SystemParameters.tm.Int, pp))

  @inline implicit def toRichExpression(e: Boolean)(implicit pp: ProgramPoint): RichExpression =
    RichExpression(Constant(e.toString, SystemParameters.tm.Int, pp))

  @inline def not(expr: Expression): Expression = {
    NegatedBooleanExpression(expr)
  }

  final case class RichExpression(expr: Expression) {

    @inline def +(other: RichExpression): RichExpression =
      BinaryNumNum(this.expr, other.expr, ArithmeticOperator.+)

    @inline def -(other: RichExpression): RichExpression =
      BinaryNumNum(this.expr, other.expr, ArithmeticOperator.-)

    @inline def *(other: RichExpression): RichExpression =
      BinaryNumNum(this.expr, other.expr, ArithmeticOperator.*)

    @inline def /(other: RichExpression): RichExpression =
      BinaryNumNum(this.expr, other.expr, ArithmeticOperator./)

    @inline def <(other: RichExpression): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.<)

    @inline def >(other: RichExpression): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.>)

    @inline def >=(other: RichExpression): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.>=)

    @inline def <=(other: RichExpression): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.<=)

    @inline def equal(other: RichExpression): RichExpression =
      if (this.expr.typ.isStringType && other.expr.typ.isStringType)
        BinaryStrBool(this.expr, other.expr, ArithmeticOperator.==)
      else
        BinaryNumBool(this.expr, other.expr, ArithmeticOperator.==)

    @inline def unequal(other: RichExpression): RichExpression =
      BinaryNumBool(this.expr, other.expr, ArithmeticOperator.!=)

    @inline def &&(other: RichExpression): RichExpression =
      BinaryBoolBool(this.expr, other.expr, BooleanOperator.&&)

    @inline def ||(other: RichExpression): RichExpression =
      BinaryBoolBool(this.expr, other.expr, BooleanOperator.||)

  }
}