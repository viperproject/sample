package ch.ethz.inf.pm.sample.abstractdomain

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

  def isComparison(op: Value): Boolean = Set(>=, <=, ==, !=, >, <) contains op

  def isArithmetic(op: Value): Boolean = !isComparison(op)

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


/**
 * Expressions are used to represents the values returned by statements. For instance, an assignment returns
 * a UnitExpression, while a variable access returns a VariableIdentifier and a field access returns a heap
 * identifier. Expressions represent the results of method calls and arithmetic and boolean operations as
 * well. 
 *
 * @author Pietro Ferrara & Lucas Brutschy
 * @since 0.1
 */
trait Expression {

  /** The type of this expression. */
  def typ: Type

  /** Point in the program where this expression is located. */
  def pp: ProgramPoint

  /** All identifiers that are part of this expression. */
  def ids: IdentifierSet

  /**
   * Replace one identifier by another in this expression (and all sub-expressions)
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

  /**
   * Runs f on the expression and all sub-expressions
   *
   * This also replaces identifiers inside heap ID sets.
   *
   * @param f the transformer
   * @return the transformed expression
   */
  def transform(f: (Expression => Expression)): Expression

  /**
   * Checks if function f evaluates to true for any sub-expression
   */
  def contains(f: (Expression => Boolean)): Boolean

  // SHORTHANDS
  def equal(that:Expression):Expression = BinaryArithmeticExpression(this,that,ArithmeticOperator.==,DummyBooleanType)
  def unequal(that:Expression):Expression = BinaryArithmeticExpression(this,that,ArithmeticOperator.!=,DummyBooleanType)

  /**
   * It was checked that this expression is simplified the following way:
   *
   * - It does not contain conjunctions or disjunctions
   * - It does not contain negations
   */
  var canonical:Boolean = false

}


/** The negation of an expression. */
case class NegatedBooleanExpression(exp: Expression) extends Expression {
  def typ = exp.typ

  def pp = exp.pp

  def ids = exp.ids

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

  def pp = thisExpr.pp

  def typ = returntyp

  def ids: IdentifierSet = thisExpr.ids ++ {
    var result: IdentifierSet = IdentifierSet.Bottom
    for (p <- parameters) {
      result ++= p.ids
    }
    result
  }

  override def hashCode(): Int = thisExpr.hashCode()

  override def equals(o: Any) = o match {
    case AbstractOperator(l, p, t, opx, ty) => thisExpr.equals(l) && parameters.equals(p) && typeparameters.equals(t) & op.equals(opx)
    case _ => false
  }

  override def toString = thisExpr.toString + "." + op.toString + ToStringUtilities.parametricTypesToString(typeparameters) + "(" + ToStringUtilities.listToString(parameters) + ")"

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
 * @param returnTyp The type of the returned value
 * @author Pietro Ferrara
 * @since 0.1
 */
case class BinaryBooleanExpression(
                                    left: Expression,
                                    right: Expression,
                                    op: BooleanOperator.Value,
                                    returnTyp: Type = SystemParameters.typ.top()) extends Expression {

  def pp = left.pp

  def typ = returnTyp

  def ids: IdentifierSet = left.ids ++ right.ids

  override def hashCode(): Int = left.hashCode()

  override def equals(o: Any) = o match {
    case BinaryBooleanExpression(l, r, opx, ty) => left.equals(l) && right.equals(r) && op.equals(opx)
    case _ => false
  }

  override def toString = left.toString + op.toString + right.toString

  override def transform(f: (Expression => Expression)): Expression =
    f(BinaryBooleanExpression(left.transform(f), right.transform(f), op, returnTyp))

  def contains(f: (Expression => Boolean)): Boolean = f(this) || left.contains(f) || right.contains(f)

}

/**
 * A comparison between reference, that is, left == right or left != right
 *
 * @param left One of the operands
 * @param right The other operand
 * @param op The identifier of the operation
 * @param returntyp The type of the returned value
 * @author Pietro Ferrara
 * @since 0.1
 */
case class ReferenceComparisonExpression(
                                          left: Expression,
                                          right: Expression,
                                          op: ArithmeticOperator.Value,
                                          returntyp: Type) extends Expression {

  require(left.typ.isObject,
    "cannot perform reference comparisons on primitive values")
  require(right.typ.isObject,
    "cannot perform reference comparisons on primitive values")

  // TODO: Maybe introduce a ReferenceOperator enum with just two values
  require(op == ArithmeticOperator.== || op == ArithmeticOperator.!=,
    "operator must either be equality or inequality")

  def pp = left.pp

  def typ = returntyp

  def ids = left.ids ++ right.ids

  override def hashCode(): Int = left.hashCode()

  override def equals(o: Any) = o match {
    case ReferenceComparisonExpression(l, r, opx, ty) => left.equals(l) && right.equals(r) && op.equals(opx)
    case _ => false
  }

  override def toString = s"$left$op$right"

  override def transform(f: (Expression => Expression)): Expression =
    f(copy(left = left.transform(f), right = right.transform(f)))

  def contains(f: (Expression => Boolean)): Boolean = f(this) || left.contains(f) || right.contains(f)

}

/**
 * A binary arithmetic expression, e.g. A1+A2 or A1>=A2
 *
 * @param left One of the operands
 * @param right The other operand
 * @param op The identifier of the operation
 * @param returntyp The type of the returned value
 * @author Pietro Ferrara
 * @since 0.1
 */
case class BinaryArithmeticExpression(
                                       left: Expression,
                                       right: Expression,
                                       op: ArithmeticOperator.Value,
                                       returntyp: Type = SystemParameters.typ.top()) extends Expression {

  def pp = if (left.pp == null) right.pp else left.pp

  def typ = returntyp

  def ids = left.ids ++ right.ids

  override def hashCode(): Int = left.hashCode()

  override def equals(o: Any) = o match {
    case BinaryArithmeticExpression(l, r, opx, ty) => left.equals(l) && right.equals(r) && op.equals(opx)
    case _ => false
  }

  override def toString = left.toString + op.toString + right.toString

  override def transform(f: (Expression => Expression)): Expression =
    f(BinaryArithmeticExpression(left.transform(f), right.transform(f), op, returntyp))

  def contains(f: (Expression => Boolean)): Boolean = f(this) || left.contains(f) || right.contains(f)

}

object BinaryArithmeticExpression {
  /**
   * Creates an expression that represents the concatenation
   * of a sequence of expressions with a certain arithmetic operator.
   *
   * @param exps the sequence of expressions to concatenate
   * @param op the arithmetic operator to concatenate the expressions with
   * @param typ the type of expressions and the resulting expressions
   * @param emptyExp the expression to return if `exps` is empty
   */
  def apply(exps: Iterable[Expression],
            op: ArithmeticOperator.Value,
            typ: Type,
            emptyExp: Expression): Expression =
    if (exps.isEmpty) emptyExp
    else exps.reduceLeft(BinaryArithmeticExpression(_, _, op, typ))
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

  def pp = left.pp

  def typ = returntyp

  def ids = left.ids

  override def hashCode(): Int = left.hashCode()

  override def equals(o: Any) = o match {
    case UnaryArithmeticExpression(l, opx, ty) => left.equals(l) && op.equals(opx)
    case _ => false
  }

  override def toString = op.toString + left.toString

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
                     typ: Type = SystemParameters.typ.top(),
                     pp: ProgramPoint = DummyProgramPoint)
  extends Expression {

  def ids = IdentifierSet.Bottom

  override def hashCode(): Int = constant.hashCode()

  override def equals(o: Any) = o match {
    case Constant(c, t, _) => constant.equals(c) && typ.equals(t)
    case _ => false
  }

  override def toString = constant

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

  override def toString = getName

  def sanitizedName = getName.replaceAll("[^a-z0-9A-Z]*","")

  def contains(f: (Expression => Boolean)): Boolean = f(this)
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

  override def equals(o: Any) = o match {
    case ProgramPointScopeIdentifier(oPP) => pp.equals(oPP)
    case _ => false
  }

  override def toString = "@" + pp.toString

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

  override def getName = name.toString + scope.toString

  override def toString = getName

  override def getField = None

  // Variables always represent exactly one concrete identifier
  override def representsSingleVariable = true
}

/**
 * The heap identifier that has to be implemented by particular heap analyses.
 */
trait HeapIdentifier[I <: HeapIdentifier[I]] extends Identifier {}

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

  override def equals(o: Any) = o match {
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

  def getName = stringPath.mkString(".")

  def getField = ???

  def typ = path.last.typ

  def pp = path.last.pp

  def representsSingleVariable: Boolean = true

  override def toString = getName

  def stringPath: List[String] =
    path.map(_.getName)

  def objPath: List[String] =
    if (typ.isObject) stringPath else stringPath.dropRight(1)
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
 *
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

}

/**
 *
 * Represents an expression with a nondeterministic operator.
 *
 * @author Lucas Brutschy
 *
 */
case class BinaryNondeterministicExpression(left: Expression, right: Expression, op: NondeterministicOperator.Value, returnType: Type) extends Expression {
  def pp = left.pp

  def typ = returnType

  def ids = left.ids ++ right.ids

  override def hashCode(): Int = left.hashCode()

  override def equals(o: Any) = o match {
    case BinaryNondeterministicExpression(l, r, op2, ty) => left.equals(l) && right.equals(r) && op.equals(op2)
    case _ => false
  }

  override def toString = left.toString + " " + op.toString + " " + right.toString

  override def transform(f: (Expression => Expression)): Expression =
    f(BinaryNondeterministicExpression(left.transform(f), right.transform(f), op, returnType))

  def contains(f: (Expression => Boolean)): Boolean = f(this) || left.contains(f) || right.contains(f)
}