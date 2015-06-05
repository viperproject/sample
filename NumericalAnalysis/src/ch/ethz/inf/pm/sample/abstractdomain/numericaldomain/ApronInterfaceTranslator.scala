package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronTools._
import ch.ethz.inf.pm.sample.abstractdomain.{ArithmeticOperator => AOp}
import ch.ethz.inf.pm.sample.oorepresentation.{Type, DummyProgramPoint}
import ch.ethz.inf.pm.sample.SystemParameters
import com.typesafe.scalalogging.LazyLogging

/**
 * Translates Apron constraints to Sample expressions.
 *
 * @param apronInterface the Apron interface to translate
 * @param resultTransformer the function applied to the resulting expressions,
 *                          DefaultExpSimplifier by default.
 * @param boolType boolean type used to generate boolean expressions
 */
case class ApronInterfaceTranslator (
    resultTransformer: (Expression => Expression) = ExpSimplifier,
    boolType: Type = SystemParameters.typ.top())
    (val apronInterface: Apron.Inner[_,_]) extends LazyLogging {

  /**
   * Translates all Apron constraints in the ApronInterface to a sequence
   * of Expressions.
   *
   * @return the equivalent expressions
   */
  def translateAll(): Set[Expression] = {
    val linearConstraints = apronInterface.apronState.toLincons(apronInterface.manager)
    linearConstraints.map(translate).flatten.toSet
  }

  /**
   * Translates a single linear Apron constraint to a Sample expression.
   * @param c the Apron constraint to translate
   * @return the constraint expressed as a Sample expression
   */
  def translate(c: Lincons1): Option[Expression] = {
    // Separate terms with positive and negative coefficients, such that we can
    // build a linear inequality whose terms only have positive coefficients.
    val nonZeroLinterms = c.getLinterms.filter(!_.getCoefficient.isZero)
    var (leftTerms, rightTerms) = nonZeroLinterms.partition(
      _.getCoefficient.cmp(new DoubleScalar(0)) > 0)

    // Negate all negative terms
    rightTerms = rightTerms.map(negateTerm)

    var leftExps = leftTerms.toList map translate
    var rightExps = rightTerms.toList map translate

    // Determine the type of expression. As a simple heuristic, just take the
    // type of the first expression
    val typ = (leftExps ++ rightExps).head.typ

    // If the left sequence of expressions is empty, flip the operator
    var op = translateOp(c.getKind)
    var const = c.getCst
    if (leftExps.isEmpty) {
      val tmpExps = leftExps
      leftExps = rightExps
      rightExps = tmpExps
      const = negateCoeff(const)
      op = ArithmeticOperator.flip(op)
    }

    // Add the constant to the LHS or RHS of the inequality,
    // depending on whether it is positive or negative
    val sampleConst = Constant(const.toString, typ, DummyProgramPoint)
    val sampleNegConst = Constant(negateCoeff(const).toString, typ, DummyProgramPoint)

    if (const.cmp(new DoubleScalar(0)) > 0) {
      if (rightExps.isEmpty) {
        // Prefer 'x = -1' over 'x + 1 = 0'
        rightExps ::= sampleNegConst
      } else {
        leftExps ::= sampleConst
      }
    } else if (const.cmp(new DoubleScalar(0)) < 0)
      rightExps ::= sampleNegConst

    val zero = Constant("0", typ, DummyProgramPoint)
    val result = BinaryArithmeticExpression(
      left = BinaryArithmeticExpression(leftExps, AOp.`+`, typ, zero),
      right = BinaryArithmeticExpression(rightExps, AOp.`+`, typ, zero),
      op = op,
      returntyp = boolType)

    if (typ.isBooleanType && (result.op != AOp.`==` && result.op != AOp.`!=`))
      None // Do not return boolean inequalities for the moment
    else
      Some(resultTransformer(result))
  }

  /**
   * Translates a single linear Apron term to a Sample expression.
   * @param t the Apron term to translate
   * @return the term expressed as a Sample expression
   */
  def translate(t: Linterm1): Expression = {
    val id = resolve(t.getVariable)
    val coeff = Constant(t.coeff.toString, id.typ, id.pp)
    BinaryArithmeticExpression(coeff, id, ArithmeticOperator.*, id.typ)
  }

  /** Translates an Apron operator to a Sample operator */
  private def translateOp(kind: Int): ArithmeticOperator.Value = kind match {
    case Lincons1.DISEQ => ArithmeticOperator.!=
    case Lincons1.EQ => ArithmeticOperator.==
    case Lincons1.SUP => ArithmeticOperator.>
    case Lincons1.SUPEQ => ArithmeticOperator.>=
  }

  /** Returns a negated version of the given coefficient */
  private def negateCoeff(c: Coeff): Coeff = {
    val negCoeff = c.copy
    negCoeff.neg()
    negCoeff
  }

  /** Returns the negated version of the given term */
  private def negateTerm(t: Linterm1): Linterm1 =
    new Linterm1(t.getVariable, negateCoeff(t.getCoefficient))

  /** Maps variable names used within Apron to Sample identifiers */
  private def resolve(variable: String): Identifier = idMap.get(variable) match {
    case Some(id) => id
    case None => sys.error(s"unknown variable $variable")
  }

  private val idMap = apronInterface.ids.getNonTop.groupBy(_.getName).map { case (k,v) =>
    if (v.size > 1) sys.error(s"non-unique identifier name $k")
    else (k, v.toList.head)
  }

  def constraintContains(c: Lincons1, variable: String): Boolean = {
    for (term <- c.getLinterms)
      if (term.getVariable.equals(variable) && !term.getCoefficient.toString.equals("0"))
        return true
    false
  }

  def toTexpr1Intern(e: Expression, env: apron.Environment): List[Texpr1Intern] = {
    val e1 = this.toTexpr1Node(e)
    for (e <- e1) yield new Texpr1Intern(env, e)
  }

  def topExpression(): Texpr1Node = new Texpr1CstNode(topInterval)

  def topConstraint(env: Environment): Tcons1 = {
    new Tcons1(Tcons1.EQ, new Texpr1Intern(env, new Texpr1CstNode(new DoubleScalar(0)))) // always true
  }

  def bottomConstraint(env: Environment): Tcons1 = {
    new Tcons1(Tcons1.EQ, new Texpr1Intern(env, new Texpr1CstNode(new DoubleScalar(1)))) // always false
  }

  def toTexpr1Node(e: Expression): List[Texpr1Node] = e match {
    case Constant("posinfty", typ, p) =>
      val a = new DoubleScalar()
      a.setInfty(1)
      List(new Texpr1CstNode(a))
    case Constant("neginfty", typ, p) =>
      val a = new DoubleScalar()
      a.setInfty(-1)
      List(new Texpr1CstNode(a))
    case x: Identifier => List(new Texpr1VarNode(x.getName))
    case setId: HeapIdSetDomain[Identifier] =>
      if (setId.isTop || setId.isBottom) List(topExpression())
      else
        (setId.value map {
          x: Identifier => new Texpr1VarNode(x.getName)
        }).toList
    case Constant(v, typ, p) =>
      if (typ.isNumericalType)
        v match {
          case "true" => List(new Texpr1CstNode(new DoubleScalar(1)))
          case "false" => List(new Texpr1CstNode(new DoubleScalar(0)))
          case _ => List(new Texpr1CstNode(new DoubleScalar(java.lang.Double.parseDouble(v))))
        }
      else List(topExpression())
    case BinaryArithmeticExpression(left, right, op, typ) =>
      for (l <- this.toTexpr1Node(left); r <- this.toTexpr1Node(right)) yield {
        this.convertArithmeticOperator(op) match {
          case Some(x) => new Texpr1BinNode(x, l, r)
          case None => topExpression()
        }
      }
    case BinaryBooleanExpression(left, right, op, typ) =>
      for (l <- this.toTexpr1Node(left); r <- this.toTexpr1Node(right)) yield {
        this.convertBooleanOperator(op) match {
          case Some(x) => new Texpr1BinNode(x, l, r)
          case None => topExpression()
        }
      }
    case UnaryArithmeticExpression(left, op, typ) =>
      op match {
        case ArithmeticOperator.- =>
          for (l <- this.toTexpr1Node(left)) yield {
            new Texpr1UnNode(Texpr1UnNode.OP_NEG, l)
          }
      }
    case _ =>
      // Naturally, not all expressions will be supported by the numerical domain
      logger.debug("Unhandled expression type in APRON interface (returning top expression): "+e)
      List(topExpression())
  }

  def convertArithmeticOperator(op: ArithmeticOperator.Value): Option[Int] = op match {
    case ArithmeticOperator.+ => Some(Texpr1BinNode.OP_ADD)
    case ArithmeticOperator.- => Some(Texpr1BinNode.OP_SUB)
    case ArithmeticOperator./ => Some(Texpr1BinNode.OP_DIV)
    case ArithmeticOperator.* => Some(Texpr1BinNode.OP_MUL)
    case ArithmeticOperator.% => Some(Texpr1BinNode.OP_MOD)
    case _ => None
  }

  /** used when we assign a boolean value, e.g. flag = flag1 && flag2 */
  def convertBooleanOperator(op: BooleanOperator.Value): Option[Int] = op match {
    case BooleanOperator.&& => Some(Texpr1BinNode.OP_MUL)
    case BooleanOperator.|| => Some(Texpr1BinNode.OP_ADD)
    case _ => None
  }

  def toTcons1(e: Expression, env: Environment): List[Tcons1] = e match {
    case BinaryArithmeticExpression(left, right, op, typ) =>
      var localOp = op
      var localLeft = left
      var localRight = right
      op match {
        case ArithmeticOperator.>= =>
        case ArithmeticOperator.== =>
        case ArithmeticOperator.!= =>
        case ArithmeticOperator.> =>
        case ArithmeticOperator.<= => localLeft = right; localRight = left; localOp = ArithmeticOperator.>=
        case ArithmeticOperator.< => localLeft = right; localRight = left; localOp = ArithmeticOperator.>
      }
      val expr1 = this.toTexpr1Node(new BinaryArithmeticExpression(localLeft, localRight, ArithmeticOperator.-, localLeft.typ))
      localOp match {
        case ArithmeticOperator.>= => for (e <- expr1) yield new Tcons1(env, Tcons1.SUPEQ, e)
        case ArithmeticOperator.== => for (e <- expr1) yield new Tcons1(env, Tcons1.EQ, e)
        case ArithmeticOperator.!= => for (e <- expr1) yield new Tcons1(env, Tcons1.DISEQ, e)
        case ArithmeticOperator.> =>

          if (apronInterface.manager.isInstanceOf[Octagon] || apronInterface.manager.isInstanceOf[OptOctagon]) {

            // Some domains, like Octagons have trouble representing >. In floating point mode, they will replace
            // A > B by A >= B, which can cause massive imprecision. We replace this by A >= B + EPSILON, where
            // EPSILON should be the smallest representable number. (actually, we are generating A - B - EPSILON >= 0)

            val sExpr1 = new BinaryArithmeticExpression(localLeft, localRight, ArithmeticOperator.-, localLeft.typ)
            val sExpr2 = new BinaryArithmeticExpression(sExpr1, Constant(NumericalAnalysisConstants.epsilon.toString, sExpr1.typ, sExpr1.pp), ArithmeticOperator.-, sExpr1.typ)
            for (e <- this.toTexpr1Node(sExpr2)) yield {
              new Tcons1(env, Tcons1.SUPEQ, e)
            }

          } else {
            for (e <- expr1) yield new Tcons1(env, Tcons1.SUP, e)
          }

      }
    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
      toTcons1(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op), typ), env)
    case NegatedBooleanExpression(NegatedBooleanExpression(x)) => toTcons1(x, env)
    case NegatedBooleanExpression(x) =>
      toTcons1(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.==, x.typ), env)
    case x: Expression =>
      toTcons1(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.!=, x.typ), env)
    case _ =>
      logger.debug("Unhandled constraint type in APRON interface (returning top constraint): "+e)
      List(topConstraint(env))
  }


}